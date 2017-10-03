'use strict';
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';

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

    function parse(xs : string[]) : [vscode.Uri, vscode.Diagnostic][] {
        let r1 = /(..[^:]+):([0-9]+):([0-9]+):/
        let r2 = /(..[^:]+):([0-9]+):([0-9]+)-([0-9]+):/
        let r3 = /(..[^:]+):\(([0-9]+),([0-9]+)\)-\(([0-9]+),([0-9]+)\):/
        var m : RegExpMatchArray;
        let f = (l1,c1,l2,c2) => {
            let range = new vscode.Range(parseInt(m[l1])-1,parseInt(m[c1])-1,parseInt(m[l2])-1,parseInt(m[c2]));
            let file = vscode.Uri.file(path.isAbsolute(m[1]) ? m[1] : path.join(dir, m[1]));
            var s = xs[0].substring(m[0].length).trim();
            let i = s.indexOf(':');
            var sev = vscode.DiagnosticSeverity.Error;
            if (i !== -1) {
                if (s.substr(0, i).toLowerCase() == 'warning')
                    sev = vscode.DiagnosticSeverity.Warning;
                s = s.substr(i+1).trim();
            }
            let msg = [].concat([s],xs.slice(1)).join('\n');
            return [pair(file, new vscode.Diagnostic(range, msg, sev))];
        };
        if (xs[0].startsWith("All good"))
            return [];
        if (m = xs[0].match(r1))
            return f(2,3,2,3);
        if (m = xs[0].match(r2))
            return f(2,3,2,4);
        if (m = xs[0].match(r3))
            return f(2,3,4,5);
        return [[new vscode.Uri(), new vscode.Diagnostic(new vscode.Range(0,0,0,0), xs.join('\n'))]];
    }
    return [].concat(... split(lines(s)).map(parse));
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


// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {
    // The command has been defined in the package.json file
    // Now provide the implementation of the command with  registerCommand
    // The commandId parameter must match the command field in package.json
    let disposable = vscode.commands.registerCommand('extension.watchGhcid', () => {
        try {
            let d = vscode.languages.createDiagnosticCollection('ghcid');
            let file = vscode.window.activeTextEditor.document.uri.fsPath;
            let last = [];
            let go = () => {
                let next = parseGhcidOutput(path.dirname(file), fs.readFileSync(file, "utf8"));
                let next2 = next.map(x => pair(x[0], [x[1]]));
                for (let x of last)
                    next2.push(pair(x[0], []));
                d.set(groupDiagnostic(next2));
                last = next;
            };
            fs.watch(file, go);
            go();
        } catch (e) {
            console.error("Ghcid extension failed: " + e);
            throw e;
        }
    });
    context.subscriptions.push(disposable);
}

// this method is called when your extension is deactivated
export function deactivate() {
}
