(self.webpackChunkwww=self.webpackChunkwww||[]).push([[2402],{22267:function(e,t,r){"use strict";r.r(t);var n=r(64649),o=r(27378),a=r(39243),i=r(55676),c=r(61005),l=r(87296);function u(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function s(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?u(Object(r),!0).forEach((function(t){(0,n.Z)(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):u(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function p(e,t){var r="undefined"!=typeof Symbol&&e[Symbol.iterator]||e["@@iterator"];if(r)return(r=r.call(e)).next.bind(r);if(Array.isArray(e)||(r=function(e,t){if(!e)return;if("string"==typeof e)return d(e,t);var r=Object.prototype.toString.call(e).slice(8,-1);"Object"===r&&e.constructor&&(r=e.constructor.name);if("Map"===r||"Set"===r)return Array.from(e);if("Arguments"===r||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(r))return d(e,t)}(e))||t&&e&&"number"==typeof e.length){r&&(e=r);var n=0;return function(){return n>=e.length?{done:!0}:{done:!1,value:e[n++]}}}throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}function d(e,t){(null==t||t>e.length)&&(t=e.length);for(var r=0,n=new Array(t);r<t;r++)n[r]=e[r];return n}var y={co_lc:{syntax:{defaultToken:"invalid",keywords:["let","in","yield","spawn","resume","if","then","else","stat","\\"],symbols:/[*\+_\{\}\|<>,\\?\->.:=!;\[\]+]|(->)/,lower:/[a-z][a-zA-Z0-9_'\w$]*/,tokenizer:{root:[[/(.*error.*)/,"error"],[/\d+/,"number"],[/@lower/,{cases:{"@keywords":"keyword","@default":"identifier"}}],[/`Pending|`Done/,"keyword"],[/[A-Z][a-zA-Z0-9_'\w$]*/,"constructor"],{include:"@whitespace"},[/: \s*/,"operator","@type"],[/[()]/,"@brackets"],[/@symbols/,{cases:{"@keywords":"keyword","@default":"operator"}}]],whitespace:[[/[ \t\r\n]+/,"white"],[/#\s+[\^]+$/,"comment"],[/#\s+[\^]+/,"comment","@type"],[/#.*$/,"comment"]],type:[[/\]$/,"keyword.type","@popall"],[/\]/,"keyword.type","@pop"],[/\[/,"keyword.type","@push"],[/\}$/,"keyword.type","@popall"],[/\}/,"keyword.type","@pop"],[/\{/,"keyword.type","@push"],[/,/,"keyword.type"],[/;/,"keyword.type"],[/[A-z][a-zA-Z]*$/,"tag","@pop"],[/[A-z][a-zA-Z]*\s/,"tag","@pop"],[/int/,"keyword.type"],[/void/,"keyword.type"],[/\s*$/,"@whitespace","@pop"],[/\s+/,"@whitespace"],[/=/,"default","@pop"]]}},hover:function(e){return function(t,r){var n=t.getValue(),o=c.hover(n,r.lineNumber,r.column);if(null===o)return null;var a=o.info,i=o.range,l=i.start,u=i.fin;return{range:new e.Range(l.line,l.col,u.line,u.col),contents:a.map((function(e){return{value:e}}))}}}},vm:{syntax:{defaultToken:"invalid",keywords:[],symbols:/[,;<>\\?\->.:=\u03BB]+/,tokenizer:{root:[[/^[.a-zA-Z0-9_$?@].*:/,{token:"type.identifier"}],[/[a-z][-_a-z0-9]*/,{token:"keyword",next:"@rest"}],[/[<=]/,{token:"keyword",next:"@rest"}],[/\d+/,"number"],{include:"@whitespace"},[/[{()}]/,"@brackets"],[/@symbols/,{cases:{"@keywords":"keyword","@default":"operator"}}]],rest:[[/^.*$/,{token:"@rematch",next:"@pop"}],[/[{}<>()[\]]/,"@brackets"],[/-?\d+/,"number"],[/[a-z][-_a-zA-Z]*/,"type.identifier"],{include:"@whitespace"}],whitespace:[[/[ \t\r\n]+/,"white"],[/(%.*$)/,"comment"]]}}}};t.default=function(){var e=function(){for(var e,t=(0,l.K2)("2477728471"),r={},n=function(){var t=e.value,n=t.relativePath.split("/").at(-1).split(".co")[0],a=o.useState(""),i=a[0],l=a[1],u="https://ayazhafiz.com";fetch(new URL(t.publicURL,u)).then((function(e){return e.text()})).then((function(e){return c.userProgram(e)})).then(l).catch((function(){console.log("failed to fetch",u,t.publicURL)})),r[n]=i},a=p(t.allFile.nodes);!(e=a()).done;)n();return r}(),t=function(e,t){for(var r,n={},o=function(){var o,a,l=r.value,u=[["emit",{value:e,options:c.emits}]],p=s({title:l,editorLanguage:null!==(o=null===(a=t[l])||void 0===a?void 0:a.editorLanguage)&&void 0!==o?o:"co_lc"},(0,i.Pt)((function(e,t){return c.compile(e,l,t)}),u));if("ir"===l){var d,y,f="eval";n[l]=[p,s({title:f,editorLanguage:null!==(d=null===(y=t[l])||void 0===y?void 0:y.editorLanguage)&&void 0!==d?d:"co_lc"},(0,i.Pt)((function(e,t){return c.compile(e,f,t)}),u))]}else n[l]=[p]},a=p(c.phases);!(r=a()).done;)o();return n}("print",{ir:{editorLanguage:"vm"}});return o.createElement(a.Z,{title:"co_lc Playground",language:"co_lc",source:"https://github.com/ayazhafiz/plts/tree/base/co_lc",grammar:"https://github.com/ayazhafiz/plts/blob/base/co_lc/ast_parser.mly",languageRegistrations:y,backends:t,defaultBackend:"ir",examples:e,defaultExample:"fib"})}},45085:function(){},75705:function(){}}]);
//# sourceMappingURL=component---src-pages-playground-co-lc-tsx-bbb3e47ebed54d5a9716.js.map