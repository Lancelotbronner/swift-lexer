// swift-tools-version: 6.2

import CompilerPluginSupport
import PackageDescription

let package = Package(
	name: "swift-lexer",
	platforms: [
		.macOS(.v15),
		.iOS(.v13),
		.tvOS(.v13),
		.watchOS(.v6),
		.macCatalyst(.v13),
	],
	products: [
		.library(name: "Lexing", targets: ["Lexing"]),
	],
	dependencies: [
		.package(url: "https://github.com/apple/swift-syntax.git", from: "602.0.0"),
		.package(url: "https://github.com/apple/swift-collections.git", from: "1.0.5"),
		.package(url: "https://github.com/swiftlang/swift-experimental-string-processing", revision: "swift-6.1.1-RELEASE"),
		.package(url: "https://github.com/apple/swift-docc-plugin", from: "1.3.0"),
	],
	targets: [
		.target(name: "RegexSyntax", dependencies: [
			.product(name: "ContainersPreview", package: "swift-collections"),
			.product(name: "BasicContainers", package: "swift-collections"),
		]),
		.macro(
			name: "LexerMacros",
			dependencies: [
				.product(name: "SwiftSyntaxMacros", package: "swift-syntax"),
				.product(name: "SwiftCompilerPlugin", package: "swift-syntax"),
				.product(name: "OrderedCollections", package: "swift-collections"),
				.product(name: "_RegexParser", package: "swift-experimental-string-processing"),
				"CoreLexer",
				"RegexSyntax",
			], swiftSettings: [
				.enableExperimentalFeature("Lifetimes"),
				.enableExperimentalFeature("InoutLifetimeDependence"),
			]),
		.target(name: "CoreLexer"),
		.target(name: "Lexing", dependencies: ["LexerMacros", "CoreLexer"]),
		.testTarget(
			name: "LexerTests",
			dependencies: [
				.product(name: "SwiftSyntaxMacrosTestSupport", package: "swift-syntax"),
				"LexerMacros",
				"CoreLexer",
				"Lexing",
			]
		),
	]
)
