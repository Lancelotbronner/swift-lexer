//
//  Containers+.swift
//  swift-lexer
//
//  Created by Christophe Bronner on 2025-12-31.
//

import ContainersPreview

extension Box: @retroactive @unchecked Sendable where T: Sendable {}
