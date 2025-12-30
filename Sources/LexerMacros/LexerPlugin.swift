import SwiftCompilerPlugin
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

@main struct LexerPlugin: CompilerPlugin {
	let providingMacros: [Macro.Type] = [
		LexerMacro.self,
		CaseMacro.self,
	]
}
