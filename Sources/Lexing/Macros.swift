@_exported import CoreLexer

// MARK: - callback type

/// Token callback type.
public typealias FillCallback<T: LexerProtocol, R> = (inout LexerMachine<T>) -> R
public typealias CreateCallback<T: LexerProtocol, R: Into<TokenResult<T>>> = (inout LexerMachine<T>) -> R

// MARK: - Macros

/// Lexer Conformance Macro
@attached(extension, conformances: LexerProtocol, Into, names: arbitrary)
public macro Lexer(skip chars: String? = nil) = #externalMacro(module: "LexerMacros", type: "LexerMacro")

/// Literal token definition, with create callback.
@attached(peer)
public macro Token<T: LexerProtocol, R>(
	_ value: String,
	priority: UInt? = nil,
	createCallback: @escaping CreateCallback<T, R>,
) = #externalMacro(module: "LexerMacros", type: "CaseMacro")

/// Literal token definition, with fill callback.
@attached(peer)
public macro Token<T: LexerProtocol, R>(
	_ value: String,
	priority: UInt? = nil,
	fillCallback: @escaping FillCallback<T, R>,
) = #externalMacro(module: "LexerMacros", type: "CaseMacro")

/// Literal token definition, without callbacks.
@attached(peer)
public macro Token(
	_ value: String,
	priority: UInt? = nil
) = #externalMacro(module: "LexerMacros", type: "CaseMacro")

/// Regex token definition, with create callback.
@attached(peer)
public macro Token<T: LexerProtocol, R, M>(
	_ value: Regex<M>,
	priority: UInt? = nil,
	createCallback: @escaping CreateCallback<T, R>,
) = #externalMacro(module: "LexerMacros", type: "CaseMacro")

/// Regex token definition, with fill callback.
@attached(peer)
public macro Token<T: LexerProtocol, R, M>(
	_ value: Regex<M>,
	priority: UInt? = nil,
	fillCallback: @escaping FillCallback<T, R>,
) = #externalMacro(module: "LexerMacros", type: "CaseMacro")

/// Regex token definition, without callbacks.
@attached(peer)
public macro Token<M>(
	_ value: Regex<M>,
	priority: UInt? = nil,
) = #externalMacro(module: "LexerMacros", type: "CaseMacro")
