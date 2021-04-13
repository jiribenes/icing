import * as monaco from 'monaco-editor';
import { loadWASM } from 'onigasm' // peer dependency of 'monaco-textmate'
import { Registry } from 'monaco-textmate' // peer dependency
import { wireTmGrammars } from './monaco-editor-textmate.ts'

export async function initHaskell(editor) {
    await loadWASM(`node_modules/onigasm/lib/onigasm.wasm`) // See https://www.npmjs.com/package/onigasm#light-it-up

    const registry = new Registry({
        getGrammarDefinition: async (scopeName) => {
            return {
                format: 'json',
                content: await (await fetch(`static/haskell.tmLanguage.json`)).text()
            }
        }
    })

    // map of monaco "language id's" to TextMate scopeNames
    const grammars = new Map()
    grammars.set('hoskell', 'source.hs')

    const theme = convertTheme(oldTheme);

    // XXX: We can redefine the original theme? Wtf.
    monaco.editor.defineTheme('vs-dark', theme);

    monaco.languages.register({ id: 'hoskell' });

    monaco.languages.setLanguageConfiguration('hoskell', {
         onEnterRules: [
            {
                beforeText: /(\bif\b(?!')(.(?!then))*|\b(then|else|m?do|of|let|in|where)\b(?!')|=|->|>>=|>=>|=<<|(^(data)( |\t)+(\w|')+( |\t)*))( |\t)*$/,
                action: { indentAction: monaco.languages.IndentAction.Indent }
            }
        ],
        wordPattern: /([\w'_][\w'_\d]*)|([0-9]+\.[0-9]+([eE][+-]?[0-9]+)?|[0-9]+[eE][+-]?[0-9]+)/
    });
    
    await wireTmGrammars(monaco, registry, grammars, editor);
}

const oldTheme : IVSCodeTheme = {
	"$schema": "vscode://schemas/color-theme",
	"type": "dark",
	"colors": {
		"activityBarBadge.background": "#007acc",
		"editor.background": "#1e1e1e",
		"editor.foreground": "#d4d4d4",
		"editor.inactiveSelectionBackground": "#3a3d41",
		"editor.selectionHighlightBackground": "#add6ff26",
		"editorIndentGuide.activeBackground": "#707070",
		"editorIndentGuide.background": "#404040",
		"input.placeholderForeground": "#a6a6a6",
		"list.dropBackground": "#383b3d",
		"menu.background": "#252526",
		"menu.foreground": "#cccccc",
		"sideBarSectionHeader.background": "#00000000",
		"sideBarSectionHeader.border": "#cccccc33",
		"sideBarTitle.foreground": "#bbbbbb",
		"statusBarItem.remoteBackground": "#16825d",
		"statusBarItem.remoteForeground": "#ffffff",
		"tab.lastPinnedBorder": "#cccccc33",
	},
	"tokenColors": [
		{
			"scope": [
				"meta.embedded",
				"source.groovy.embedded"
			],
			"settings": {
				"foreground": "#D4D4D4"
			}
		},
		{
			"scope": "emphasis",
			"settings": {
				"fontStyle": "italic"
			}
		},
		{
			"scope": "strong",
			"settings": {
				"fontStyle": "bold"
			}
		},
		{
			"scope": "header",
			"settings": {
				"foreground": "#000080"
			}
		},
		{
			"scope": "comment",
			"settings": {
				"foreground": "#6A9955"
			}
		},
		{
			"scope": "constant.language",
			"settings": {
				"foreground": "#569CD6"
			}
		},
		{
			"scope": [
				"constant.numeric",
				"variable.other.enummember",
				"keyword.operator.plus.exponent",
				"keyword.operator.minus.exponent"
			],
			"settings": {
				"foreground": "#B5CEA8"
			}
		},
		{
			"scope": "constant.regexp",
			"settings": {
				"foreground": "#646695"
			}
		},
		{
			"scope": "entity.name.tag",
			"settings": {
				"foreground": "#569CD6"
			}
		},
		{
			"scope": "entity.name.tag.css",
			"settings": {
				"foreground": "#D7BA7D"
			}
		},
		{
			"scope": "entity.other.attribute-name",
			"settings": {
				"foreground": "#9CDCFE"
			}
		},
		{
			"scope": [
				"entity.other.attribute-name.class.css",
				"entity.other.attribute-name.class.mixin.css",
				"entity.other.attribute-name.id.css",
				"entity.other.attribute-name.parent-selector.css",
				"entity.other.attribute-name.pseudo-class.css",
				"entity.other.attribute-name.pseudo-element.css",
				"source.css.less entity.other.attribute-name.id",
				"entity.other.attribute-name.scss"
			],
			"settings": {
				"foreground": "#D7BA7D"
			}
		},
		{
			"scope": "invalid",
			"settings": {
				"foreground": "#F44747"
			}
		},
		{
			"scope": "markup.underline",
			"settings": {
				"fontStyle": "underline"
			}
		},
		{
			"scope": "markup.bold",
			"settings": {
				"foreground": "#569CD6",
				"fontStyle": "bold"
			}
		},
		{
			"scope": "markup.heading",
			"settings": {
				"foreground": "#569CD6",
				"fontStyle": "bold"
			}
		},
		{
			"scope": "markup.italic",
			"settings": {
				"fontStyle": "italic"
			}
		},
		{
			"scope": "markup.inserted",
			"settings": {
				"foreground": "#B5CEA8"
			}
		},
		{
			"scope": "markup.deleted",
			"settings": {
				"foreground": "#CE9178"
			}
		},
		{
			"scope": "markup.changed",
			"settings": {
				"foreground": "#569CD6"
			}
		},
		{
			"scope": "punctuation.definition.quote.begin.markdown",
			"settings": {
				"foreground": "#6A9955"
			}
		},
		{
			"scope": "punctuation.definition.list.begin.markdown",
			"settings": {
				"foreground": "#6796E6"
			}
		},
		{
			"scope": "markup.inline.raw",
			"settings": {
				"foreground": "#CE9178"
			}
		},
		{
			"scope": "punctuation.definition.tag",
			"settings": {
				"foreground": "#808080"
			}
		},
		{
			"scope": [
				"meta.preprocessor",
				"entity.name.function.preprocessor"
			],
			"settings": {
				"foreground": "#569CD6"
			}
		},
		{
			"scope": "meta.preprocessor.string",
			"settings": {
				"foreground": "#CE9178"
			}
		},
		{
			"scope": "meta.preprocessor.numeric",
			"settings": {
				"foreground": "#B5CEA8"
			}
		},
		{
			"scope": "meta.structure.dictionary.key.python",
			"settings": {
				"foreground": "#9CDCFE"
			}
		},
		{
			"scope": "meta.diff.header",
			"settings": {
				"foreground": "#569CD6"
			}
		},
		{
			"scope": "storage",
			"settings": {
				"foreground": "#569CD6"
			}
		},
		{
			"scope": "storage.type",
			"settings": {
				"foreground": "#569CD6"
			}
		},
		{
			"scope": [
				"storage.modifier",
				"keyword.operator.noexcept"
			],
			"settings": {
				"foreground": "#569CD6"
			}
		},
		{
			"scope": [
				"string",
				"meta.embedded.assembly"
			],
			"settings": {
				"foreground": "#CE9178"
			}
		},
		{
			"scope": "string.tag",
			"settings": {
				"foreground": "#CE9178"
			}
		},
		{
			"scope": "string.value",
			"settings": {
				"foreground": "#CE9178"
			}
		},
		{
			"scope": "string.regexp",
			"settings": {
				"foreground": "#D16969"
			}
		},
		{
			"scope": [
				"punctuation.definition.template-expression.begin",
				"punctuation.definition.template-expression.end",
				"punctuation.section.embedded"
			],
			"settings": {
				"foreground": "#569CD6"
			}
		},
		{
			"scope": [
				"meta.template.expression"
			],
			"settings": {
				"foreground": "#D4D4D4"
			}
		},
		{
			"scope": [
				"support.type.vendored.property-name",
				"support.type.property-name",
				"variable.css",
				"variable.scss",
				"variable.other.less",
				"source.coffee.embedded"
			],
			"settings": {
				"foreground": "#9CDCFE"
			}
		},
		{
			"scope": "keyword",
			"settings": {
				"foreground": "#569CD6"
			}
		},
		{
			"scope": "keyword.control",
			"settings": {
				"foreground": "#569CD6"
			}
		},
		{
			"scope": "keyword.operator",
			"settings": {
				"foreground": "#D4D4D4"
			}
		},
		{
			"scope": [
				"keyword.operator.new",
				"keyword.operator.expression",
				"keyword.operator.cast",
				"keyword.operator.sizeof",
				"keyword.operator.alignof",
				"keyword.operator.typeid",
				"keyword.operator.alignas",
				"keyword.operator.instanceof",
				"keyword.operator.logical.python",
				"keyword.operator.wordlike"
			],
			"settings": {
				"foreground": "#569CD6"
			}
		},
		{
			"scope": "keyword.other.unit",
			"settings": {
				"foreground": "#B5CEA8"
			}
		},
		{
			"scope": [
				"punctuation.section.embedded.begin.php",
				"punctuation.section.embedded.end.php"
			],
			"settings": {
				"foreground": "#569CD6"
			}
		},
		{
			"scope": "support.function.git-rebase",
			"settings": {
				"foreground": "#9CDCFE"
			}
		},
		{
			"scope": "constant.sha.git-rebase",
			"settings": {
				"foreground": "#B5CEA8"
			}
		},
		{
			"scope": [
				"storage.modifier.import.java",
				"variable.language.wildcard.java",
				"storage.modifier.package.java"
			],
			"settings": {
				"foreground": "#D4D4D4"
			}
		},
		{
			"scope": "variable.language",
			"settings": {
				"foreground": "#569CD6"
			}
		},
		{
			"scope": [
				"entity.name.function",
				"support.function",
				"support.constant.handlebars",
				"source.powershell variable.other.member",
				"entity.name.operator.custom-literal"
			],
			"settings": {
				"foreground": "#DCDCAA"
			}
		},
		{
			"scope": [
				"meta.return-type",
				"support.class",
				"support.type",
				"entity.name.type",
				"entity.name.namespace",
				"entity.other.attribute",
				"entity.name.scope-resolution",
				"entity.name.class",
				"storage.type.numeric.go",
				"storage.type.byte.go",
				"storage.type.boolean.go",
				"storage.type.string.go",
				"storage.type.uintptr.go",
				"storage.type.error.go",
				"storage.type.rune.go",
				"storage.type.cs",
				"storage.type.generic.cs",
				"storage.type.modifier.cs",
				"storage.type.variable.cs",
				"storage.type.annotation.java",
				"storage.type.generic.java",
				"storage.type.java",
				"storage.type.object.array.java",
				"storage.type.primitive.array.java",
				"storage.type.primitive.java",
				"storage.type.token.java",
				"storage.type.groovy",
				"storage.type.annotation.groovy",
				"storage.type.parameters.groovy",
				"storage.type.generic.groovy",
				"storage.type.object.array.groovy",
				"storage.type.primitive.array.groovy",
				"storage.type.primitive.groovy"
			],
			"settings": {
				"foreground": "#4EC9B0"
			}
		},
		{
			"scope": [
				"meta.type.cast.expr",
				"meta.type.new.expr",
				"support.constant.math",
				"support.constant.dom",
				"support.constant.json",
				"entity.other.inherited-class"
			],
			"settings": {
				"foreground": "#4EC9B0"
			}
		},
		{
			"scope": [
				"keyword.control",
				"source.cpp keyword.operator.new",
				"keyword.operator.delete",
				"keyword.other.using",
				"keyword.other.operator",
				"entity.name.operator"
			],
			"settings": {
				"foreground": "#C586C0"
			}
		},
		{
			"scope": [
				"variable",
				"meta.definition.variable.name",
				"support.variable",
				"entity.name.variable"
			],
			"settings": {
				"foreground": "#9CDCFE"
			}
		},
		{
			"scope": [
				"variable.other.constant",
				"variable.other.enummember"
			],
			"settings": {
				"foreground": "#4FC1FF"
			}
		},
		{
			"scope": [
				"meta.object-literal.key"
			],
			"settings": {
				"foreground": "#9CDCFE"
			}
		},
		{
			"scope": [
				"support.constant.property-value",
				"support.constant.font-name",
				"support.constant.media-type",
				"support.constant.media",
				"constant.other.color.rgb-value",
				"constant.other.rgb-value",
				"support.constant.color"
			],
			"settings": {
				"foreground": "#CE9178"
			}
		},
		{
			"scope": [
				"punctuation.definition.group.regexp",
				"punctuation.definition.group.assertion.regexp",
				"punctuation.definition.character-class.regexp",
				"punctuation.character.set.begin.regexp",
				"punctuation.character.set.end.regexp",
				"keyword.operator.negation.regexp",
				"support.other.parenthesis.regexp"
			],
			"settings": {
				"foreground": "#CE9178"
			}
		},
		{
			"scope": [
				"constant.character.character-class.regexp",
				"constant.other.character-class.set.regexp",
				"constant.other.character-class.regexp",
				"constant.character.set.regexp"
			],
			"settings": {
				"foreground": "#D16969"
			}
		},
		{
			"scope": [
				"keyword.operator.or.regexp",
				"keyword.control.anchor.regexp"
			],
			"settings": {
				"foreground": "#DCDCAA"
			}
		},
		{
			"scope": "keyword.operator.quantifier.regexp",
			"settings": {
				"foreground": "#D7BA7D"
			}
		},
		{
			"scope": "constant.character",
			"settings": {
				"foreground": "#569CD6"
			}
		},
		{
			"scope": "constant.character.escape",
			"settings": {
				"foreground": "#D7BA7D"
			}
		},
		{
			"scope": "entity.name.label",
			"settings": {
				"foreground": "#C8C8C8"
			}
		},
		{
			"scope": "token.info-token",
			"settings": {
				"foreground": "#6796E6"
			}
		},
		{
			"scope": "token.warn-token",
			"settings": {
				"foreground": "#CD9731"
			}
		},
		{
			"scope": "token.error-token",
			"settings": {
				"foreground": "#F44747"
			}
		},
		{
			"scope": "token.debug-token",
			"settings": {
				"foreground": "#B267E6"
			}
		}
	]
};



export interface IVSCodeTheme {
    "$schema": "vscode://schemas/color-theme",
    "type": 'dark' | 'light',
    colors: { [name: string]: string };
    tokenColors: {
        name?: string;
        "scope": string[] | string,
        "settings": {
            foreground?: string;
            background?: string;
            fontStyle?: string;
        }
    }[]
}

export type IMonacoThemeRule = monaco.editor.ITokenThemeRule[]

export function convertTheme(theme: IVSCodeTheme): monaco.editor.IStandaloneThemeData {

    const monacoThemeRule: IMonacoThemeRule = [];
    const returnTheme: monaco.editor.IStandaloneThemeData = {
        inherit: false,
        base: 'vs-dark',
        colors: theme.colors,
        rules: monacoThemeRule,
        encodedTokensColors: []
    };

    theme.tokenColors.map((color) => {

        if (typeof color.scope == 'string') {

            const split = color.scope.split(',');

            if (split.length > 1) {
                color.scope = split;
                evalAsArray();
                return;
            }



            monacoThemeRule.push(Object.assign({}, color.settings, {
                // token: color.scope.replace(/\s/g, '')
                token: color.scope
            }));
            return;
        }

        evalAsArray();

        function evalAsArray() {
            (color.scope as string[]).map((scope) => {
                monacoThemeRule.push(Object.assign({}, color.settings, {
                    token: scope
                }));
            });
        }
    });

    monacoThemeRule.push({ token: '', foreground: 'D4D4D4' });

    return returnTheme;
}
