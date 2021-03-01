import * as monaco from 'monaco-editor';

export const initProlog = () => {
	monaco.languages.register({ id: 'swi-prolog', aliases: ['swiprolog', 'swipl', 'prolog'] });


	monaco.languages.setLanguageConfiguration('swi-prolog', {
		indentationRules: indentationRules,
		wordPattern: wordPattern,
		onEnterRules: onEnterRules,
	});

	//monaco.languages.setMonarchTokensProvider('swi-prolog', {
	//	//operators: operators,
	//	//keywords: keywords,
	//	//tokenizer: tokenizer,
	//	brackets: brackets,
	//});

	console.log("Prolog registered!");
}

const indentationRules = {
	decreaseIndentPattern: /(\s*\)|\s*\])$/,
	increaseIndentPattern: /(.*:-\s*|.*-->\s*|.*:->\s*|.*:<-\s*|.+\[|.+\()$/,
};

const wordPattern = /(-?\d*\.\d\w*)|([^\`\~\!\@\#\%\^\&\*\(\)\-\=\+\[\{\]\}\\\|\;\:\'\"\,\.\<\>\/\?\s]+)/g;

const brackets: monaco.languages.IMonarchLanguageBracket[] = [
	{ open: ')', close: ')', token: 'parens' }
];

const onEnterRules = [
        // {
        //   beforeText: /.+:-|:- begin_tests.+\.$/,
        //   action: { indentAction: IndentAction.Indent }
        // },
        {
          beforeText: /(^\s*|.*%.+)$/,
          action: { indentAction: monaco.languages.IndentAction.None }
        },
        {
          beforeText: /.+\.$/,
          action: { indentAction: monaco.languages.IndentAction.Outdent }
        },
        {
          beforeText: /.+\([^\)]*$/,
          action: { indentAction: monaco.languages.IndentAction.Indent }
        },
        // {
        //   beforeText: /.+\[[^\]]*$/,
        //   action: { indentAction: monaco.languages.IndentAction.Indent }
        // },
        {
          // e.g. /** | */
          beforeText: /^\s*\/\*\*(?!\/)([^\*]|\*(?!\/))*$/,
          afterText: /^\s*\*\/$/,
          action: {
            indentAction: monaco.languages.IndentAction.IndentOutdent,
            appendText: " * "
          }
        },
        {
          // e.g. /** ...|
          beforeText: /^\s*\/\*\*(?!\/)([^\*]|\*(?!\/))*$/,
          action: { indentAction: monaco.languages.IndentAction.None, appendText: " * " }
        },
        {
          // e.g.  * ...|
          beforeText: /^(\t|(\ \ ))*\ \*(\ ([^\*]|\*(?!\/))*)?$/,
          action: { indentAction: monaco.languages.IndentAction.None, appendText: "* " }
        },
        {
          // e.g.  */|
          beforeText: /^(\t|(\ \ ))*\ \*\/\s*$/,
          action: { indentAction: monaco.languages.IndentAction.None, removeText: 1 }
        },
        {
          // e.g.  *-----*/|
          beforeText: /^(\t|(\ \ ))*\ \*[^/]*\*\/\s*$/,
          action: { indentAction: monaco.languages.IndentAction.None, removeText: 1 }
        }
		];

const keywords = [ 'not', 'fail' ];

const operators = [
		'.', ',', ':-', '\\+', '='
	];

//const tokenizer = {
//	root: [
//		[/(?<![a-zA-Z0-9_])[a-z][a-zA-Z0-9_]*(?!\\s*\\(|[a-zA-Z0-9_])/, 'atom']
//		//{ include: '@whitespace' },
//		//[ /[a-zA-Z_][\w_]*('*)/, {
//		//  cases: {
//		//	  '@keywords': 'keyword',
//		//	  '@default': 'identifier'
//		//  } } ],
//		//[ ':-', 'keyword' ]
//	],
//
//};
