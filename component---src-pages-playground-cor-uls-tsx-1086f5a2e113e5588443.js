(self.webpackChunkwww=self.webpackChunkwww||[]).push([[5478],{16844:function(e,t,r){"use strict";var n=r(64649),o=r(27378),a=r(39243),i=r(55676),l=r(22253),u=r(87296);function c(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function s(e,t){var r="undefined"!=typeof Symbol&&e[Symbol.iterator]||e["@@iterator"];if(r)return(r=r.call(e)).next.bind(r);if(Array.isArray(e)||(r=function(e,t){if(!e)return;if("string"==typeof e)return p(e,t);var r=Object.prototype.toString.call(e).slice(8,-1);"Object"===r&&e.constructor&&(r=e.constructor.name);if("Map"===r||"Set"===r)return Array.from(e);if("Arguments"===r||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(r))return p(e,t)}(e))||t&&e&&"number"==typeof e.length){r&&(e=r);var n=0;return function(){return n>=e.length?{done:!0}:{done:!1,value:e[n++]}}}throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}function p(e,t){(null==t||t>e.length)&&(t=e.length);for(var r=0,n=new Array(t);r<t;r++)n[r]=e[r];return n}function f(e,t,r){for(var o,a={},u=function(){var u,s,p=o.value,f=[["emit",{value:t,options:l.emits}]],d=function(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?c(Object(r),!0).forEach((function(t){(0,n.Z)(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):c(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}({title:p,editorLanguage:null!==(u=null===(s=r[p])||void 0===s?void 0:s.editorLanguage)&&void 0!==u?u:e},(0,i.Pt)((function(t,r){return l.compile(t,e,p,r)}),f));a[p]=[d]},p=s(l.phases);!(o=p()).done;)u();return a}t.Z=function(e){var t,r=e.experiment,n=e.defaultPhase,i=e.defaultEmit,c=e.backendOverrides,p=void 0===c?{}:c,d=e.languageRegistrations,y=void 0===d?{}:d;y[r]&&(y[r].hover=(t=r,function(e){return function(r,n){var o=r.getValue(),a=l.hover(o,t,n.lineNumber,n.column),i=a.info,u=a.range,c=u.start,s=u.fin;return{range:new e.Range(c.line,c.col,s.line,s.col),contents:i.map((function(e){return{value:e}}))}}}));for(var g,b={},m=s((0,u.K2)("1214908427").allFile.nodes);!(g=m()).done;){var h=g.value;if(h.relativePath.includes("/"+r+"/")){var w=h.relativePath.split("/").at(-1).split(".roc")[0],v=o.useState(""),k=v[0],O=v[1],z="https://ayazhafiz.com";console.log("getting",z,h.publicURL),fetch(new URL(h.publicURL,z)).then((function(e){return e.text()})).then((function(e){return l.userProgram(e)})).then(O),b[w]=k}}return o.createElement(a.Z,{title:"cor/"+r+" Playground",language:r,source:"https://github.com/ayazhafiz/cor/tree/base/experiments/"+r,grammar:"https://github.com/ayazhafiz/cor/blob/base/experiments/"+r+"/parser.mly",languageRegistrations:y,backends:f(r,i,p),defaultBackend:n,examples:b,defaultExample:Object.keys(b)[0]})}},71099:function(e,t,r){"use strict";r.r(t);var n,o=r(82769),a=r(16844),i=((n={}).uls={syntax:{defaultToken:"invalid",keywords:["entry","let","in","choice","\\"],symbols:/[_\{\}\|<>\\?\->.:=!;\[\]+]|(->)/,lower:/[a-z][a-zA-Z0-9_'\w$]*/,tokenizer:{root:[[/(.*error.*)/,"error"],[/proto\s*/,"keyword","@proto"],[/@lower/,{cases:{"@keywords":"keyword","@default":"identifier"}}],[/[A-Z][a-zA-Z0-9_'\w$]*/,"constructor"],{include:"@whitespace"},[/[()]/,"@brackets"],[/`\d+/,"tag"],[/~\d+/,"tag"],[/@symbols/,{cases:{"@keywords":"keyword","@default":"operator"}}]],proto:[[/@lower/,"identifier","@protoArg"]],protoArg:[[/@lower/,"type","@protoArg"],[/:/,"operator","@type"]],whitespace:[[/[ \t\r\n]+/,"white"],[/#\s+[\^]+$/,"comment"],[/#\s+[\^]+/,"comment","@type"],[/#.*$/,"comment"]],type:[[/\(\)$/,"keyword.type","@popall"],[/\(\)/,"keyword.type"],[/->|[\+,]/,"operator"],[/-\[/,"type"],[/\]->$/,"type","@popall"],[/\]->/,"type"],[/([`?]\d+)$/,"tag","@popall"],[/([`?]\d+)/,"tag"],[/(~\d+)(:)([a-z]+|\?\d+)(:)([a-z][a-zA-Z0-9]*)/,["tag","operator","type","operator","identifier"]],[/[a-zA-Z][a-zA-Z0-9_']*$/,"type","@popall"],[/[a-zA-Z][a-zA-Z0-9_']*/,"type"],[/[()\[\]]$/,"@brackets","@popall"],[/[()\[\]]/,"@brackets"],[/[ \t]*$/,"@whitespace","@popall"],[/[ \t]+/,"@whitespace"]]}}},n);t.default=function(e){return(0,o.Z)(e),(0,a.Z)({experiment:"uls",defaultPhase:"solve",defaultEmit:"elab",languageRegistrations:i})}},21913:function(){},38569:function(){},82769:function(e,t,r){"use strict";function n(e){if(null==e)throw new TypeError("Cannot destructure undefined")}r.d(t,{Z:function(){return n}})}}]);
//# sourceMappingURL=component---src-pages-playground-cor-uls-tsx-1086f5a2e113e5588443.js.map