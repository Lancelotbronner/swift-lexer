//
//  KaleidoscopeBuilder.swift
//
//
//  Created by Larry Zeng on 11/26/23.
//

import SwiftCompilerPlugin
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros

let LEXER_PACKAGE_NAME: String = "Lexing"

let LEXER_MACRO_NAME: String = "Lexer"
let LEXER_TOKEN_NAME: String = "Token"
let LEXER_MACRO_SKIP_ATTR: String = "skip"

let LEXER_PRIORITY_OPTION: String = "priority"
let LEXER_FILL_CALLBACK_OPTION: String = "fillCallback"
let LEXER_CREATE_CALLBACK_OPTION: String = "createCallback"

/// This extension macro generates an extension to the decorated enum and make it conform to the lexer protocol
/// so that the decorated enum can be a tokenizer.
public struct LexerMacro: ExtensionMacro {
	public static func expansion(of node: SwiftSyntax.AttributeSyntax, attachedTo declaration: some SwiftSyntax.DeclGroupSyntax, providingExtensionsOf type: some SwiftSyntax.TypeSyntaxProtocol, conformingTo protocols: [SwiftSyntax.TypeSyntax], in context: some SwiftSyntaxMacros.MacroExpansionContext) throws -> [SwiftSyntax.ExtensionDeclSyntax] {
		guard let enumDecl = declaration.as(EnumDeclSyntax.self) else {
			throw LexerError.NotAnEnum
		}

		// get enum identity
		let enumIdent = enumDecl.name.text

		// generate graph
		var graph = Graph()

		// get the macro lists
		let lexingMacroDelcList = enumDecl.attributes.filter { attr in
			let attrName: TypeSyntax? = attr.as(AttributeSyntax.self)?.attributeName
			let ident = attrName?.as(IdentifierTypeSyntax.self)?.name.text
			let member = attrName?.as(MemberTypeSyntax.self)
			let memberIdent = member?.name.text
			let memberType = member?.baseType.as(IdentifierTypeSyntax.self)?.name.text

			// check how many of the attributes are like
			// @Lexer() or Lexing.Lexer()
			return ident == LEXER_MACRO_NAME || (memberIdent == LEXER_MACRO_NAME && memberType == LEXER_PACKAGE_NAME)
		}

		// if there are more than 1,
		// throw error to indicate duplication
		if lexingMacroDelcList.count > 1 {
			throw LexerError.MultipleMacroDecleration
		}

		if let lexingAttrs = lexingMacroDelcList[0].as(AttributeSyntax.self)?.arguments?.as(LabeledExprListSyntax.self) {
			for lexingAttr in lexingAttrs {
				switch lexingAttr.label?.text {
				case LEXER_MACRO_SKIP_ATTR:
					guard let skipString = lexingAttr.expression.as(StringLiteralExprSyntax.self)?.segments.description else {
						throw LexerError.ExpectingString
					}
					try graph.push(input: .init(token: "SKIP_REGEX_TOKEN", tokenType: .skip, hir: HIR(regex: skipString)))
				case _:
					break
				}
			}
		}

		// get the member case xxx blocks
		for member in enumDecl.memberBlock.members {
			guard let caseDecl = member.decl.as(EnumCaseDeclSyntax.self) else {
				continue
			}

			// summarize `case  A, B, C(K.String), D(String, Int)`
			// get (name, paramTypes) tuples
			// for example, (A, nil), (C, ["K.String"])
			let caseTypes: [(name: String, paramTypes: [String]?)] = caseDecl.elements.map { element in
				(name: element.name.text, paramTypes: element.parameterClause?.parameters.map { $0.type.description })
			}

			var attrMatches: [AttrMatchInfo] = []

			// parse attributes
			for attr in caseDecl.attributes {
				guard let attr = attr.as(AttributeSyntax.self) else {
					continue
				}

				let attrName: TypeSyntax? = attr.attributeName
				let ident = attrName?.as(IdentifierTypeSyntax.self)?.name.text
				let member = attrName?.as(MemberTypeSyntax.self)
				let memberIdent = member?.name.text
				let memberType = member?.baseType.as(IdentifierTypeSyntax.self)?.name.text

				switch (ident, memberIdent, memberType) {
				case (LEXER_TOKEN_NAME, _, _), (_, LEXER_TOKEN_NAME, LEXER_PACKAGE_NAME):
					try attrMatches.append(parse(attr))
				case _:
					break
				}
			}

			for token in caseTypes {
				for (hir, tokenType, priority) in attrMatches {
					try graph.push(input: .init(token: token.name, tokenType: tokenType, hir: hir, priority: priority))
				}
			}
		}

		_ = try graph.makeRoot()
		let rootId = try graph.shake()

		var generator = Generator(graph: graph, enumIdent: enumIdent)

		let lexerConformance: DeclSyntax = try """
		extension \(raw: enumIdent): LexerProtocol {
			public typealias TokenType = Self
			public typealias RawSource = String
		
			public static func lex(_ lexer: inout LexerMachine<Self>) throws {
				\(raw: generator.buildFunctions())
		
				try \(raw: generator.generateFuncIdent(nodeId: rootId))(&lexer)
			}
		
			public static func lexer(source: RawSource) -> LexerMachine<Self> {
				return LexerMachine(raw: source)
			}
		}
		"""

		let tokenResultConformance: DeclSyntax = """
		extension \(raw: enumIdent): Into {
			public typealias IntoType = TokenResult<\(raw: enumIdent)>
			public func into() -> IntoType {
				return .result(self)
			}
		}
		"""

		return [
			lexerConformance.cast(ExtensionDeclSyntax.self),
			tokenResultConformance.cast(ExtensionDeclSyntax.self)
		]
	}
}

typealias AttrMatchInfo = (regex: HIR, type: TokenType, priority: UInt?)

extension SyntaxCollection {
	subscript(index: Int) -> Element {
		return self[self.index(startIndex, offsetBy: index)]
	}
}

/// Parses an enum case's information.
func parse(_ attr: AttributeSyntax) throws -> AttrMatchInfo {
	guard let arguments = attr.arguments?.as(LabeledExprListSyntax.self) else {
		throw LexerError.ParsingError
	}

	// TODO: this might be wrong
	let expr = arguments[0].expression
	let hir: HIR

	if case let .regexLiteralPattern(pattern) = expr.as(RegexLiteralExprSyntax.self)?.regex.tokenKind {
		hir = try HIR(regex: pattern)
	} else if let token = expr.as(StringLiteralExprSyntax.self)?.segments.description {
		hir = try HIR(token: token)
	} else {
		throw LexerError.ExpectingString
	}

	var matchCallback: TokenType = .standalone
	var priority: UInt? = nil

	if let foundExpr = findExpression(LEXER_FILL_CALLBACK_OPTION, in: arguments)?.expression {
		if let lambda = foundExpr.as(ClosureExprSyntax.self) {
			// TODO: this might be wrong
			matchCallback = .fillCallback(.Lambda(lambda.description))
		} else {
			// TODO: this might be wrong
			matchCallback = .fillCallback(.Named(foundExpr.description))
		}
	}

	if let foundExpr = findExpression(LEXER_CREATE_CALLBACK_OPTION, in: arguments)?.expression {
		if case .standalone = matchCallback {
			if let lambda = foundExpr.as(ClosureExprSyntax.self) {
				// TODO: this might be wrong
				matchCallback = .createCallback(.Lambda(lambda.description))
			} else {
				// TODO: this might be wrong
				matchCallback = .createCallback(.Named(foundExpr.description))
			}
		} else {
			throw LexerError.OnlyFillOrCreateCallbackIsAllowed
		}
	}

	if let foundExpr = findExpression(LEXER_PRIORITY_OPTION, in: arguments)?.expression {
		guard let num = foundExpr.as(IntegerLiteralExprSyntax.self) else {
			throw LexerError.ExpectingIntegerLiteral
		}
		priority = UInt(num.literal.text)
	}

	return (hir, matchCallback, priority)
}

func findExpression(_ name: String, in exprList: LabeledExprListSyntax) -> LabeledExprSyntax? {
	for labeledExpr in exprList {
		if labeledExpr.label?.text == name {
			return labeledExpr
		}
	}

	return nil
}
