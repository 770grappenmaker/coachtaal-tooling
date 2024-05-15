import { workspace, ExtensionContext, commands } from 'vscode';
import { join } from 'path';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	console.log("Activated Coach extension.");

	const jarPath = context.asAbsolutePath(join("build", "lsp.jar"));
	const args = ["-jar", jarPath];
	const serverOptions: ServerOptions = {
		run: { transport: TransportKind.stdio, command: "java", args },
		debug: { transport: TransportKind.stdio, command: "java", args }
	};

	const clientOptions: LanguageClientOptions = {
		documentSelector: [
			{ scheme: 'file', language: 'plaintext', pattern: "**/*.coach" },
			{ scheme: 'file', language: 'json', pattern: "**/*.coach.json" },
		]
	};

	client = new LanguageClient(
		'coachLanguageClient',
		'Coach Launguage Client',
		serverOptions,
		clientOptions
	);

	context.subscriptions.push(commands.registerCommand("coach-lsp.reload", () => {
		client.stop().then(() => client.start());
	}));

	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) return undefined;
	return client.stop();
}