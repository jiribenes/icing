import * as monaco from 'monaco-editor';

export const initProlog = () => {
	monaco.languages.register({ id: 'swi-prolog', aliases: ['swiprolog', 'swipl', 'prolog'] });


	monaco.languages.setLanguageConfiguration('swi-prolog', {
		indentationRules: indentationRules,
		wordPattern: wordPattern,
		onEnterRules: onEnterRules,
    autoClosingPairs: [
      { open: '(', close: ')' },
      { open: '[', close: ']' }
    ],
    brackets: [
      ['(', ')'],
      ['[', ']']
    ],
    comments: {
      lineComment: '%',
      blockComment: ["/*", "*/"]
    }
	});

	monaco.languages.setMonarchTokensProvider('swi-prolog', {
		//operators: operators,
		//keywords: keywords,
		tokenizer: tokenizer,
	});

	console.log("Prolog registered!");
}

const indentationRules = {
	decreaseIndentPattern: /(\s*\)|\s*\])$/,
	increaseIndentPattern: /(.*:-\s*|.*-->\s*|.*:->\s*|.*:<-\s*|.+\[|.+\()$/,
};

const wordPattern = /(-?\d*\.\d\w*)|([^\`\~\!\@\#\%\^\&\*\(\)\-\=\+\[\{\]\}\\\|\;\:\'\"\,\.\<\>\/\?\s]+)/g;

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

const tokenizer : any= {
  root: [
    { include: '@whitespace' },
		[ /[A-Z][a-zA-Z_0-9]*/, 'constant'],
    [ ':-', 'keyword' ],
    [ ';', 'keyword' ],
    [ ',', 'keyword' ],
    [ /->/, 'keyword' ],
    [ /\./, 'keyword' ],
    [ /".*"/, { token: 'string', log: 'found string $0 in state $S0' } ]
  ],

  whitespace: [
    [ /%.*$/, 'comment' ] 
  ],
};
