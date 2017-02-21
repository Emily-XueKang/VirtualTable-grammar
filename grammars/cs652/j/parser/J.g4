grammar J;

//straring point for parsing a java file
file
    :    classDeclaration* main EOF
    ;

main
    :    mainblock*
    ;

mainblock
    :    localVariableDeclarationStatement
    |    mainstatement
    |    typeDeclaration
    ;
mainstatement
    :   'if' parExpression mainstatement ('else' mainstatement)?
    |   'for' '(' forControl ')' mainstatement
    |   'while' parExpression mainstatement
    |   'do' mainstatement 'while' parExpression ';'
    |   'return' expression? ';'
    |   'throw' expression ';'
    |   'break' Identifier? ';'
    |   'continue' Identifier? ';'
    |   ';'
    |   statementExpression ';'
    |   Identifier ':' mainstatement
    |   printstatement ';'
    ;
classDeclaration
    :    'class' Identifier /*typeParameters?*/
         ('extends' typeType)?
    //('implements' typeList)? //no interface implementation in this grammar for now
         classBody
    ;

classBody
    :   '{' classBodyDeclaration* '}'
    ;
classBodyDeclaration
    :   ';'
    |   /*'static'?*/ block
    |    memberDeclaration
    ;
memberDeclaration
    :   methodDeclaration
    |   genericMethodDeclaration
    |   fieldDeclaration
   // |   constructorDeclaration
   // |   genericConstructorDeclaration
   // |   interfaceDeclaration
    |   annotationTypeDeclaration
    |   classDeclaration
    |   enumDeclaration
    ;
/*interfaceDeclaration
    :   'interface' Identifier typeParameters? ('extends' typeList)? interfaceBody
    ;
*/
enumDeclaration
    :   ENUM Identifier ('implements' typeList)?
        '{' enumConstants? ','? enumBodyDeclarations? '}'
    ;

enumConstants
    :   enumConstant (',' enumConstant)*
    ;

enumConstant
    :   annotation* Identifier arguments? classBody?
    ;

enumBodyDeclarations
    :   ';' classBodyDeclaration*
    ;

annotation
    :   '@' annotationName ( '(' ( elementValuePairs | elementValue )? ')' )?
    ;

annotationName : qualifiedName ;

qualifiedName
    :   Identifier ('.' Identifier)*
    ;

elementValuePairs
    :   elementValuePair (',' elementValuePair)*
    ;

elementValuePair
    :   Identifier '=' elementValue
    ;

elementValue
    :   expression
    |   annotation
    |   elementValueArrayInitializer
    ;

elementValueArrayInitializer
    :   '{' (elementValue (',' elementValue)*)? (',')? '}'
    ;

arguments
    :   '(' expressionList? ')'
    ;
expressionList
    :   expression (',' expression)*
    ;


methodDeclaration
    :   (typeType|'void') Identifier formalParameters ('[' ']')*
        ('throws' qualifiedNameList)?
        (   methodBody
        |   ';'
        )
    ;
formalParameters
    :   '(' formalParameterList? ')'
    ;

formalParameterList
    :   formalParameter (',' formalParameter)* (',' lastFormalParameter)?
    |   lastFormalParameter
    ;

formalParameter
    :   variableModifier* typeType variableDeclaratorId
    ;
lastFormalParameter
    :   variableModifier* typeType '...' variableDeclaratorId
    ;
variableModifier
    :   'final'
    |   annotation
    ;

qualifiedNameList
    :   qualifiedName (',' qualifiedName)*
    ;

methodBody
    :   block
    ;

genericMethodDeclaration
    :   typeParameters methodDeclaration
    ;

fieldDeclaration
    :   typeType variableDeclarators ';'
    ;
variableDeclarators
    :   variableDeclarator (',' variableDeclarator)*
    ;
variableDeclarator
    :   variableDeclaratorId ('=' variableInitializer)?
    ;

variableDeclaratorId
    :   Identifier ('[' ']')*
    ;

variableInitializer
    :   arrayInitializer
    |   expression
    ;


primary
    :   '(' expression ')'
    |   'this'
    |   'super'
    |   literal
    |   Identifier
    |   typeType '.' 'class'
    |   'void' '.' 'class'
    |   nonWildcardTypeArguments (explicitGenericInvocationSuffix | 'this' arguments)
    ;

nonWildcardTypeArguments
    :   '<' typeList '>'
    ;
explicitGenericInvocationSuffix
    :   'super' superSuffix
    |   Identifier arguments
    ;
superSuffix
    :   arguments
    |   '.' Identifier arguments?
    ;

literal
    :   IntegerLiteral
    |   FloatingPointLiteral
    |   StringLiteral
    |   'null'
    ;
//fragment
IntegerLiteral
    :   '0'
    |   [1-9][0-9]*
    ;
//fragment
FloatingPointLiteral
    :   IntegerLiteral '.' IntegerLiteral?
    ;

StringLiteral
    :   '"' StringCharacters? '"'
    ;
fragment
StringCharacters
    :   StringCharacter+
    ;
fragment
StringCharacter
    :   ~["\\]
    |   EscapeSequence
    ;
fragment
EscapeSequence
    :   '\\' [btnfr"'\\]
    ;


arrayInitializer
    :   '{' (variableInitializer (',' variableInitializer)* (',')? )? '}'
    ;


typeParameters
    :   '<' typeParameter (',' typeParameter)* '>'
    ;
typeParameter
    :   Identifier ('extends' typeBound)?
    ;

typeBound
    :   typeType ('&' typeType)*
    ;

typeList
    :   typeType (',' typeType)*
    ;


typeType
    :   classOrInterfaceType ('[' ']')*
    |   primitiveType ('[' ']')*
    ;

classOrInterfaceType
    :   Identifier typeArguments? ('.' Identifier typeArguments? )*
    ;
typeArguments
    :   '<' typeArgument (',' typeArgument)* '>'
    ;

typeArgument
    :   typeType
    |   '?' (('extends' | 'super') typeType)?
    ;


block
    :   '{' blockStatement* '}'
    ;

blockStatement
    :   localVariableDeclarationStatement
    |   statement
    |   typeDeclaration
    ;

typeDeclaration
    :   classOrInterfaceModifier* classDeclaration
    |   classOrInterfaceModifier* enumDeclaration
//    |   classOrInterfaceModifier* interfaceDeclaration
    |   classOrInterfaceModifier* annotationTypeDeclaration
    |   ';'
    ;
modifier
    :   classOrInterfaceModifier
    |   (   'native'
        |   'synchronized'
        |   'transient'
        |   'volatile'
        )
    ;

// Annotation type
annotationTypeDeclaration
    :   '@' 'interface' Identifier annotationTypeBody
    ;

annotationTypeBody
    :   '{' (annotationTypeElementDeclaration)* '}'
    ;

annotationTypeElementDeclaration
    :   modifier* annotationTypeElementRest
    |   ';' // this is not allowed by the grammar, but apparently allowed by the actual compiler
    ;

annotationTypeElementRest
    :   typeType annotationMethodOrConstantRest ';'
    |   classDeclaration ';'?
//    |   interfaceDeclaration ';'?
    |   enumDeclaration ';'?
    |   annotationTypeDeclaration ';'?
    ;

annotationMethodOrConstantRest
    :   annotationMethodRest
    |   annotationConstantRest
    ;

annotationMethodRest
    :   Identifier '(' ')' defaultValue?
    ;

annotationConstantRest
    :   variableDeclarators
    ;
/*Interface body
interfaceBody
    :   '{' interfaceBodyDeclaration* '}'
    ;
interfaceBodyDeclaration
    :   modifier* interfaceMemberDeclaration
    |   ';'
    ;
interfaceMemberDeclaration
    :   constDeclaration
    |   interfaceMethodDeclaration
    |   genericInterfaceMethodDeclaration
    |   interfaceDeclaration
    |   annotationTypeDeclaration
    |   classDeclaration
    |   enumDeclaration
    ;
*/

defaultValue
    :   'default' elementValue
    ;
classOrInterfaceModifier
    :   annotation       // class or interface
    |   (   'public'     // class or interface
        |   'protected'  // class or interface
        |   'private'    // class or interface
        |   'static'     // class or interface
        |   'abstract'   // class or interface
        |   'final'      // class only -- does not apply to interfaces
        |   'strictfp'   // class or interface
        )
    ;

statement
    :   block
    //|   ASSERT expression (':' expression)? ';'
    |   'if' parExpression statement ('else' statement)?
    |   'for' '(' forControl ')' statement
    |   'while' parExpression statement
    |   'do' statement 'while' parExpression ';'
//    |   'try' block (catchClause+ finallyBlock? | finallyBlock)
//    |   'try' resourceSpecification block catchClause* finallyBlock?
//    |   'switch' parExpression '{' switchBlockStatementGroup* switchLabel* '}'
//    |   'synchronized' parExpression block
    |   'return' expression? ';'
    |   'throw' expression ';'
    |   'break' Identifier? ';'
    |   'continue' Identifier? ';'
    |   ';'
    |   statementExpression ';'
    |   Identifier ':' statement
    |   printstatement ';'
    ;
printstatement
    :   'printf(' StringLiteral (',' printargs)? ')'
    ;
//printcontent
//    :   literal '\n'
//    |   printtype '\n'
//    ;
//printtype
//    :   ('%d')? ('%f')?
//    ;
printargs
    :   expression
    ;



parExpression
    :   '(' expression ')'
    ;
forControl
    :   enhancedForControl
    |   forInit? ';' expression? ';' forUpdate?
    ;
forInit
    :   localVariableDeclaration
    |   expressionList
    ;
enhancedForControl
    :   variableModifier* typeType variableDeclaratorId ':' expression
    ;

forUpdate
    :   expressionList
    ;
statementExpression
    :   expression
    ;

expression
    :   primary
    |   expression '.' Identifier
    |   expression '.' 'this'
    |   expression '.' 'new' nonWildcardTypeArguments? innerCreator
    |   expression '.' 'super' superSuffix
    |   expression '.' explicitGenericInvocation
    |   expression '[' expression ']'
    |   expression '(' expressionList? ')'
    |   'new' creator
    |   '(' typeType ')' expression
    |   expression ('++' | '--')
    |   ('+'|'-'|'++'|'--') expression
    |   ('~'|'!') expression
    |   expression ('*'|'/'|'%') expression
    |   expression ('+'|'-') expression
    |   expression ('<' '<' | '>' '>' '>' | '>' '>') expression
    |   expression ('<=' | '>=' | '>' | '<') expression
    |   expression 'instanceof' typeType
    |   expression ('==' | '!=') expression
    |   expression '&' expression
    |   expression '^' expression
    |   expression '|' expression
    |   expression '&&' expression
    |   expression '||' expression
    |   expression '?' expression ':' expression
    |   <assoc=right> expression
        (   '='
        |   '+='
        |   '-='
        |   '*='
        |   '/='
        |   '&='
        |   '|='
        |   '^='
        |   '>>='
        |   '>>>='
        |   '<<='
        |   '%='
        )
        expression
    ;
creator
    :   nonWildcardTypeArguments createdName classCreatorRest
    |   createdName (arrayCreatorRest | classCreatorRest)
    ;

createdName
    :   Identifier typeArgumentsOrDiamond? ('.' Identifier typeArgumentsOrDiamond?)*
    |   primitiveType
    ;

innerCreator
    :   Identifier nonWildcardTypeArgumentsOrDiamond? classCreatorRest
    ;

arrayCreatorRest
    :   '['
        (   ']' ('[' ']')* arrayInitializer
        |   expression ']' ('[' expression ']')* ('[' ']')*
        )
    ;

classCreatorRest
    :   arguments classBody?
    ;

typeArgumentsOrDiamond
    :   '<' '>'
    |   typeArguments
    ;
nonWildcardTypeArgumentsOrDiamond
    :   '<' '>'
    |   nonWildcardTypeArguments
    ;

explicitGenericInvocation
    :   nonWildcardTypeArguments? explicitGenericInvocationSuffix
    ;
localVariableDeclarationStatement
    :    localVariableDeclaration ';'
    ;

localVariableDeclaration
    :   variableModifier* typeType variableDeclarators
    ;
primitiveType
    :   'boolean'
    |   'char'
    |   'byte'
    |   'short'
    |   'int'
    |   'long'
    |   'float'
    |   'double'
    ;


Identifier
    :   [a-zA-Z_][a-zA-Z_0-9]*
    ;

WS  :  [ \t\r\n\u000C]+ -> skip
    ;

COMMENT
    :   '/*' .*? '*/' -> channel(HIDDEN)
    ;

LINE_COMMENT
    :   '//' ~[\r\n]* -> channel(HIDDEN)
    ;