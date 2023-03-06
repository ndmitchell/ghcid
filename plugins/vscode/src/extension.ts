'use strict';
import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';
import * as crypto from 'crypto'

function pair<a,b>(a : a, b : b) : [a,b] {return [a,b];}

export function parseGhcidOutput(dir : string, s : string) : [vscode.Uri, vscode.Diagnostic][] {
    // standard lines, dealing with \r\n as well
    function lines(s : string) : string[] {
        return s.replace('\r','').split('\n').filter(x => x != "");
    }

    // After the file location, message bodies are indented (perhaps prefixed by a line number)
    function isMessageBody(x : string) {
        if (x.startsWith(" "))
            return true;
        let sep = x.indexOf('|');
        if (sep == -1)
            return false;
        return !isNaN(Number(x.substr(0, sep)));
    }

    // split into separate error messages, which all start at col 0 (no spaces) and are following by message bodies
    function split(xs : string[]) : string[][] {
        var cont = [];
        var res = [];
        for (let x of xs) {
            if (isMessageBody(x))
                cont.push(x);
            else {
                if (cont.length > 0) res.push(cont);
                cont = [x];
            }
        }
        if (cont.length > 0) res.push(cont);
        return res;
    }

    function clean(lines: string[]): string[] {
        const newlines: string[] = []
        for (const line of lines) {
            if (/In the/.test(line)) break

            if (line.match(/\s*\|$/)) break
            if (line.match(/(\d+)?\s*\|/)) break

            newlines.push(line)
        }
        return newlines
    }

    function dedent(lines: string[]): string[] {
        const indentation = Math.min(...lines.filter(line => line !== '').map(line => line.match(/^\s*/)[0].length))
        return lines.map(line => line.slice(indentation))
    }

    function parse(xs : string[]) : [vscode.Uri, vscode.Diagnostic][] {
        let r1 = /(..[^:]+):([0-9]+):([0-9]+):/
        let r2 = /(..[^:]+):([0-9]+):([0-9]+)-([0-9]+):/
        let r3 = /(..[^:]+):\(([0-9]+),([0-9]+)\)-\(([0-9]+),([0-9]+)\):/
        var m : RegExpMatchArray;
        let mkDiagnostic = (range: vscode.Range): [vscode.Uri, vscode.Diagnostic] => {
            const file = m[1].replace(/\\/g, '/');
            let uri = vscode.Uri.file(path.isAbsolute(file) ? file : path.join(dir, file));
            var s = xs[0].substring(m[0].length).trim();
            let i = s.indexOf(':');
            var sev = vscode.DiagnosticSeverity.Error;
            if (i !== -1) {
                if (s.substr(0, i).toLowerCase() == 'warning')
                    sev = vscode.DiagnosticSeverity.Warning;
                s = s.substr(i+1).trim();
            }
            let msg = [].concat(/^\s*$/.test(s) ? [] : [s], xs.slice(1));
            return pair(uri, new vscode.Diagnostic(range, dedent(msg).join('\n'), sev));
        };
        if (xs[0].startsWith("All good"))
            return [];
        if (m = xs[0].match(r1))
            return [mkDiagnostic(new vscode.Range(parseInt(m[2])-1,parseInt(m[3])-1,parseInt(m[2])-1,parseInt(m[3])))];
        if (m = xs[0].match(r2))
            return [mkDiagnostic(new vscode.Range(parseInt(m[2])-1,parseInt(m[3])-1,parseInt(m[2])-1,parseInt(m[4])))];
        if (m = xs[0].match(r3))
            return [mkDiagnostic(new vscode.Range(parseInt(m[2])-1,parseInt(m[3])-1,parseInt(m[4])-1,parseInt(m[5])))];
        return [[vscode.Uri.parse('untitled:ghcid-errors'), new vscode.Diagnostic(new vscode.Range(0,0,0,0), dedent(xs).join('\n'))]];
    }
    return [].concat(... split(lines(s)).map(clean).map(parse));
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
