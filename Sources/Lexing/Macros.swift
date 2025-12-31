@_exported import CoreLexer

// MARK: - callback type

/// Token callback type.
public typealias FillCallback<T: LexerProtocol, R> = (inout LexerMachine<T>) -> R
public typealias CreateCallback<T: LexerProtocol, R: Into<TokenResult<T>>> = (inout LexerMachine<T>) -> R

// MARK: - Enum Case Decorators

/// Token definition macro, with a fill callback
@attached(peer)
public macro token<T: LexerProtocol, R>(_ value: String, priority: UInt? = nil, fillCallback: @escaping FillCallback<T, R>) = #externalMacro(module: "LexerMacros", type: "CaseMacro")

/// Token Definition macro, with a create callback
@attached(peer)
public macro token<T: LexerProtocol, R: Into<TokenResult<T>>>(_ value: String, priority: UInt? = nil, createCallback: @escaping CreateCallback<T, R>) = #externalMacro(module: "LexerMacros", type: "CaseMacro")

/// Token definition macro, without a callback
@attached(peer)
public macro token(_ value: String, priority: UInt? = nil) = #externalMacro(module: "LexerMacros", type: "CaseMacro")

/// Token regex definition macro, with a callback
@attached(peer)
public macro regex<T: LexerProtocol, R, M>(_ value: Regex<M>, priority: UInt? = nil, fillCallback: @escaping FillCallback<T, R>) = #externalMacro(module: "LexerMacros", type: "CaseMacro")

/// Token regex definition macro, with a callback
@attached(peer)
public macro regex<T: LexerProtocol, R: Into<TokenResult<T>>, M>(_ value: Regex<M>, priority: UInt? = nil, createCallback: @escaping CreateCallback<T, R>) = #externalMacro(module: "LexerMacros", type: "CaseMacro")

/// Token regex definition macro, without a callback
@attached(peer)
public macro regex<M>(_ value: Regex<M>, priority: UInt? = nil) = #externalMacro(module: "LexerMacros", type: "CaseMacro")

// MARK: - Enum Builder

/// Lexer Conformance Macro
@attached(extension, conformances: LexerProtocol, Into, names: arbitrary)
public macro Lexer(skip chars: String? = nil) = #externalMacro(module: "LexerMacros", type: "LexerMacro")
