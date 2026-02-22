'use strict';
import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';
import * as crypto from 'crypto'

function pair<a,b>(a : a, b : b) : [a,b] {return [a,b];}

export function parseGhcidOutput(dir: string, s: string): [vscode.Uri, vscode.Diagnostic][] {
    const allLines = s.replace(/\r/g, '').split('\n')

    if (allLines.length === 0) return []
    if (allLines[0].startsWith('All good')) return []
    if (allLines[0].startsWith('Ghcid has stopped.')) return []

    const isHeader = (line: string) => /: (error|warning):/.test(line);

    // Group lines into messages, each starting with a location header
    const messages: string[][] = [];
    for (const line of allLines) {
        if (isHeader(line)) {
            messages.push([line]);
        } else if (messages.length > 0) {
            messages[messages.length - 1].push(line);
        }
    }

    return messages.flatMap(lines => {
        const header = lines[0];
        let m: RegExpMatchArray;
        let file: string;
        let range: vscode.Range;
        let severityStr: string|undefined;

        // file:(line,col)-(line,col)
        if (m = header.match(/^(.*):\((\d+),(\d+)\)-\((\d+),(\d+)\): (error|warning):/)) {
            file = m[1];
            range = new vscode.Range(+m[2] - 1, +m[3] - 1, +m[4] - 1, +m[5]);
            severityStr = m[6];
        // file:line:col-col
        } else if (m = header.match(/^(.*):(\d+):(\d+)-(\d+): (error|warning):/)) {
            file = m[1];
            range = new vscode.Range(+m[2] - 1, +m[3] - 1, +m[2] - 1, +m[4]);
            severityStr = m[5];
        // file:line:col
        } else if (m = header.match(/^(.*):(\d+):(\d+): (error|warning):/)) {
            file = m[1];
            range = new vscode.Range(+m[2] - 1, +m[3] - 1, +m[2] - 1, +m[3]);
            severityStr = m[4];

            // Infer a better range from ^^^ caret annotations if present
            for (let i = 1; i < lines.length; i++) {
                const gutter = lines[i].match(/^\d+ \| /);
                if (gutter && i + 1 < lines.length) {
                    const carets = lines[i + 1].match(/\^+/);
                    if (carets) {
                        const start = carets.index - gutter[0].length;
                        const end = start + carets[0].length;
                        range = new vscode.Range(+m[2] - 1, start, +m[2] - 1, end);
                        break;
                    }
                }
            }
        } else {
            return [];
        }

        // Determine severity
        let severity = vscode.DiagnosticSeverity.Error;
        if (severityStr === 'warning') severity = vscode.DiagnosticSeverity.Warning;

        // Build message body
        const bodyLines: string[] = [];
        for (let i = 1; i < lines.length; i++) {
            const line = lines[i];
            if (/^\s+\|$/.test(line)) break; // code snipped
            if (/In the/.test(line)) break; // expression context
            if (/In a stmt/.test(line)) break; // statement context
            bodyLines.push(line);
        }

        // Dedent
        const nonEmpty = bodyLines.filter(l => l !== '');
        if (nonEmpty.length > 0) {
            const indent = Math.min(...nonEmpty.map(l => l.match(/^\s*/)[0].length));
            for (let i = 0; i < bodyLines.length; i++)
                bodyLines[i] = bodyLines[i].slice(indent);
        }

        file = m[1].replace(/\\/g, '/'); // normalize Windows paths
        const uri = vscode.Uri.file(path.isAbsolute(file) ? file : path.join(dir, file));
        return [pair(uri, new vscode.Diagnostic(range, bodyLines.join('\n'), severity))];
    });
}

function groupDiagnostic(xs : [vscode.Uri, vscode.Diagnostic[]][]) : [vscode.Uri, vscode.Diagnostic[]][] {
    let seen = new Map<string, [number, vscode.Uri, vscode.Diagnostic[]]>();
    for (var i = 0; i < xs.length; i++) {
        let key = xs[i][0].path;
        if (seen.has(key)) {
            let v = seen.get(key);
            v[2] = v[2].concat(xs[i][1]);
        }
        else
            seen.set(key, [i, xs[i][0], xs[i][1]]);
    }
    return Array.from(seen.values()).sort((a,b) => a[0] - b[0]).map(x => pair(x[1],x[2]));
}

function watchOutput(root : string, file : string) : fs.FSWatcher {
    let d = vscode.languages.createDiagnosticCollection('ghcid');
    let go = () => {
        d.clear()
        d.set(groupDiagnostic(parseGhcidOutput(root, fs.readFileSync(file, "utf8")).map(x => pair(x[0], [x[1]]))));
    };
    let watcher = fs.watch(file, go);
    go();
    return watcher;
}

async function autoWatch(context: vscode.ExtensionContext) {
    // TODO support multiple roots
    const watcher = vscode.workspace.createFileSystemWatcher('**/ghcid.txt')
    context.subscriptions.push(watcher);
    const uri2diags = new Map<string, vscode.DiagnosticCollection>()
    context.subscriptions.push({ dispose: () => Array.from(uri2diags.values()).forEach(diag => diag.dispose()) });

    const onUpdate = (uri: vscode.Uri) => {
        const diags = uri2diags.get(uri.fsPath) || vscode.languages.createDiagnosticCollection()
        uri2diags.set(uri.fsPath, diags)
        diags.clear()
        diags.set(groupDiagnostic(parseGhcidOutput(path.dirname(uri.fsPath), fs.readFileSync(uri.fsPath, "utf8")).map(x => pair(x[0], [x[1]]))));
    }

    (await vscode.workspace.findFiles('**/ghcid.txt')).forEach(onUpdate)
    watcher.onDidCreate(onUpdate)
    watcher.onDidChange(onUpdate)
    watcher.onDidDelete(uri => {
        uri2diags.get(uri.fsPath)?.dispose()
        uri2diags.delete(uri.fsPath)
    })
}

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export async function activate(context: vscode.ExtensionContext) {
    // The command has been defined in the package.json file
    // Now provide the implementation of the command with  registerCommand
    // The commandId parameter must match the command field in package.json

    // Pointer to the last running watcher, so we can undo it
    var oldWatcher : fs.FSWatcher = null;
    var oldTerminal : vscode.Terminal = null;

    let cleanup = () => {
        if (oldWatcher != null)
            oldWatcher.close();
        oldWatcher = null;
        if (oldTerminal != null)
            oldTerminal.dispose();
        oldTerminal = null;
    }
    context.subscriptions.push({dispose: cleanup});

    let add = (name : string, act : () => fs.FSWatcher) => {
        let dispose = vscode.commands.registerCommand(name, () => {
            try {
                cleanup();
                oldWatcher = act();
            }
            catch (e) {
                console.error("Ghcid extension failed in " + name + ": " + e);
                throw e;
            }
        });
        context.subscriptions.push(dispose);
    }

    add('extension.startGhcid', () => {
        if (!vscode.workspace.rootPath) {
            vscode.window.showWarningMessage("You must open a workspace first.")
            return null;
        }
        // hashing the rootPath ensures we create a finite number of temp files
        var hash = crypto.createHash('sha256').update(vscode.workspace.rootPath).digest('hex').substring(0, 20);
        let file = path.join(os.tmpdir(), "ghcid-" + hash + ".txt");
        context.subscriptions.push({dispose: () => {try {fs.unlinkSync(file);} catch (e) {};}});
        fs.writeFileSync(file, "");

        let ghcidCommand : string = vscode.workspace.getConfiguration('ghcid').get('command');

        let opts : vscode.TerminalOptions = {
            name: "ghcid",
            shellPath: os.type().startsWith("Windows") ? "cmd.exe" : ghcidCommand,
            shellArgs: [...(os.type().startsWith("Windows") ? ["/k", ghcidCommand] : []), "--outputfile=" + file]
        }
        oldTerminal = vscode.window.createTerminal(opts);
        oldTerminal.show();
        return watchOutput(vscode.workspace.rootPath, file);
    });

    await autoWatch(context)
}

// this method is called when your extension is deactivated
export function deactivate() {
}
