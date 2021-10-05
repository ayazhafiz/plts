(self.webpackChunkwww=self.webpackChunkwww||[]).push([[1191],{69048:function(e,t,n){"use strict";n.d(t,{W:function(){return r}});var r=function(e,t){return function(n){return function(r,a){return function(e,t,n,r,a){for(var i=a.lineNumber,o=a.column,u=r.getLineMaxColumn(i),s=r.getValueInRange(new e.Range(i,r.getLineMinColumn(i),i,u)),l=e.editor.tokenize(r.getValue(),t)[i-1],p=0;p<l.length;++p){var c=l[p].offset+1,f=l[p+1]?l[p+1].offset+1:u;if(o>=c&&o<f){var y=s.substring(c-1,f-1),m=new e.Range(i,c,i,f),d=n(y);return d&&{range:m,contents:d}}}return null}(n,e,t,r,a)}}}},29557:function(e,t,n){"use strict";n.r(t);var r=n(649),a=n(27378),i=n(21237),o=n(69048),u=n(60175),s=n(13673),l=n(76698);function p(e,t){var n="undefined"!=typeof Symbol&&e[Symbol.iterator]||e["@@iterator"];if(n)return(n=n.call(e)).next.bind(n);if(Array.isArray(e)||(n=function(e,t){if(!e)return;if("string"==typeof e)return c(e,t);var n=Object.prototype.toString.call(e).slice(8,-1);"Object"===n&&e.constructor&&(n=e.constructor.name);if("Map"===n||"Set"===n)return Array.from(e);if("Arguments"===n||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n))return c(e,t)}(e))||t&&e&&"number"==typeof e.length){n&&(e=n);var r=0;return function(){return r>=e.length?{done:!0}:{done:!1,value:e[r++]}}}throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}function c(e,t){(null==t||t>e.length)&&(t=e.length);for(var n=0,r=new Array(t);n<t;n++)r[n]=e[n];return r}var f={Queries:"\n⊥ ?? ⊤\nP∧Q ?? P∨Q\nP∧(Q∨R) ?? (P∧Q)∨(P∧R)∨(P∧Q∧R)\n(A1→A2)∧(B1→B2) ?? A1∧B1→A2∧B2\n(A1→B)∧(A2→B) ?? A1∨A2→B\n(A∨B)∧Int ?? A∨(Int∧B)\nInt ?? Double\n".trim()},y={Check:[{title:"Judgements",do:(0,u.Fr)(l.judge),options:[["Print Derivations",!0],["Prettify Symbols",!0]],editorLanguage:"ho21"}]},m={defaultToken:"invalid",constant_types:["!","*","⊤","⊥"],queries:["??"],judgements:["<:",":>","~=","#","≺","≻","≅"],operators:["&","|","->","∧","∨","→"],symbols:/[=><!~?:&|+\-*\/\^%#\u22A4\u22A5\u2227\u2228\u2192\u227A\u227B\u2245]+/,tokenizer:{root:[[/(Syntax error.*)/,"error"],[/(Parse error.*)/,"error"],[/[A-Z][a-zA-Z0-9_'\w$]*/,"type.identifier"],{include:"@whitespace"},[/[()]/,"@brackets"],[/@symbols/,{cases:{"@constant_types":"keyword","@queries":"annotation","@judgements":"keyword","@operators":"operator"}}]],whitespace:[[/(\uFF5C.*$)/,"annotation"],[/[ \t\r\n]+/,"white"],[/(--.*$)/,"comment"],[/(\\.*$)/,"annotation"]]}},d=[{aliases:["!","⊥"],kind:"⊥",text:"The bottom type",binary:!1},{aliases:["*","⊤"],kind:"⊤",text:"The top type",binary:!1},{aliases:["&","∧"],kind:"Operator",text:"An intersection type",binary:!0},{aliases:["|","∨"],kind:"Operator",text:"A union type",binary:!0},{aliases:["->","→"],kind:"Operator",text:"An arrow (function) type",binary:!0},{aliases:["??"],kind:"Query",text:"A judgement query for the relationship between A and B, which will be answered in the output editor.",binary:!0},{aliases:["<:","≺"],kind:"Judgement",text:"`S <: T` means `S` is a subtype of `T`",outputOnly:!0},{aliases:[":>","≻"],kind:"Judgement",text:"`T :> S` means `T` is a supertype of `S`",outputOnly:!0},{aliases:["~=","≅"],kind:"Judgement",text:"`T ~= S` means `T` is isomorphic to `S`",outputOnly:!0},{aliases:["#"],kind:"Judgement",text:"`T # U` means `T` and `U` are incomparable",outputOnly:!0}];var h,g=a.createElement(s.Z,{children:(h=d.filter((function(e){return!e.outputOnly})).map((function(e){var t=e.aliases,n=e.text,r=e.binary;return"- "+t.map((function(e){return"`"+(r?"A"+e+"B":e)+"`"})).join("/")+": "+n})),h.push("- Other types: Identifiers beginning with an uppercase letter are treated as primitive types.j"),h.join("\n")+"\n[Full Parser Specification](https://github.com/ayazhafiz/plts/blob/base/huang_oliveira_21/parser.mly)")});var b,v={ho21:{syntax:m,hover:(0,o.W)("ho21",(function(e){for(var t,n=p(d);!(t=n()).done;)for(var r,a=t.value,i=a.aliases,o=a.text,u=p(i);!(r=u()).done;){if(e===r.value){var s=[{value:o}];return i.length>1&&s.push({value:"*Aliases*: "+i.join(", ")}),s}}return e[0]===e[0].toUpperCase()&&e[0]!==e[0].toLowerCase()?[{value:"**Primitive**"},{value:"Type primitive `"+e+"`"}]:null})),format:function(e){return[{text:l.formatQueries(e.getValue(),!0),range:e.getFullModelRange()}]},autoFormat:{format:function(e){return[{text:l.formatQueries(e.getValue(),!0),range:e.getFullModelRange()}]},triggerCharacters:(b=[],b.push.apply(b,(0,r.Z)("abcdefghijklmnopqrstuvwxyz".split(""))),b.push.apply(b,(0,r.Z)("ABCDEFGHIJKLMNOPQRSTUVWXYZ".split(""))),b.push.apply(b,(0,r.Z)("0123456789".split(""))),b.push.apply(b,(0,r.Z)("()_'".split(""))),b.push.apply(b,(0,r.Z)(m.constant_types.join("").split(""))),b.push.apply(b,(0,r.Z)(m.queries.join("").split(""))),b.push.apply(b,(0,r.Z)(m.judgements.join("").split(""))),b.push.apply(b,(0,r.Z)(m.operators.join("").split(""))),b)}}};t.default=function(){return a.createElement(i.Z,{language:"ho21",source:"https://github.com/ayazhafiz/plts/tree/base/ho21",grammar:g,languageRegistrations:v,backends:y,defaultBackend:"Check",examples:f,defaultExample:"Queries"})}},6300:function(){},1534:function(){}}]);
//# sourceMappingURL=component---src-pages-playground-ho-21-tsx-76a6c94fdb7e02be4960.js.map