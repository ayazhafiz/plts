"use strict";(self.webpackChunkwww=self.webpackChunkwww||[]).push([[4809],{86946:function(e,a,t){t.r(a),t.d(a,{_frontmatter:function(){return o},default:function(){return u}});var n=t(30808),p=(t(27378),t(81368)),r=t(70778),i=t(70851),l=["components"],o={},s={_frontmatter:o},m=r.Z;function u(e){var a=e.components,t=(0,n.Z)(e,l);return(0,p.kt)(m,Object.assign({},s,t,{components:a,mdxType:"MDXLayout"}),(0,p.kt)("h1",null,"plts ",(0,p.kt)(i.Z,{mdxType:"Revision"})),(0,p.kt)("p",null,"Implementations of type systems and programming languages I find interesting."),(0,p.kt)("p",null,"Repository: ",(0,p.kt)("a",{parentName:"p",href:"https://github.com/ayazhafiz/plts"},"gh:ayazhafiz/plts"),"."),(0,p.kt)("h2",null,"Introductory"),(0,p.kt)("ul",null,(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("p",{parentName:"li"},(0,p.kt)("a",{parentName:"p",href:"https://ayazhafiz.com/articles/21/typescript-type-system-lambda-calculus"},"Emulating the Lambda Calculus in TypeScript's Type System"),": Evaluating the lambda calculus entirely using the TypeScript type system.")),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("p",{parentName:"li"},(0,p.kt)("a",{parentName:"p",href:"https://github.com/ayazhafiz/plts/blob/base/tapl"},"TAPL"),": Selected\nimplementations of languages formalized in ",(0,p.kt)("em",{parentName:"p"},"Types and Programming Languages"),"\n(Pierce 2002).")),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("p",{parentName:"li"},(0,p.kt)("a",{parentName:"p",href:"https://ayazhafiz.com/plts/playground/tiger"},"Tiger"),": A compiler for the Tiger Language of Appel's 1998 ",(0,p.kt)("a",{parentName:"p",href:"https://www.cs.princeton.edu/~appel/modern/ml/index.html"},"Modern\nCompiler Implementation"),"."))),(0,p.kt)("h2",null,"Roc"),(0,p.kt)("p",null,(0,p.kt)("a",{parentName:"p",href:"https://github.com/ayazhafiz/cor"},"cor")," is a minimalization of\n",(0,p.kt)("a",{parentName:"p",href:"https://roc-lang.org"},"Roc")," used for experimenting on the language and its\ncompiler."),(0,p.kt)("ul",null,(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("p",{parentName:"li"},(0,p.kt)("a",{parentName:"p",href:"https://github.com/ayazhafiz/cor/tree/base/experiments/uls"},"cor/uls"),': A\nlanguage with "unspecialized lambda sets", a novel extension of the\nHindley-Milner type system that supports efficient resolution of ad-hoc\npolymorphic usages (a-la typeclasses) during unification.'),(0,p.kt)("ul",{parentName:"li"},(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://www.notion.so/rwx/Non-linear-monomorphization-0b26991a028949a285ca77a8ffcff3c5#1930c4eadf08465f9c7b96469f11f664"},"Documentation")),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://ayazhafiz.com/plts/playground/cor/uls"},"Playground")))),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("p",{parentName:"li"},(0,p.kt)("a",{parentName:"p",href:"https://github.com/ayazhafiz/cor/tree/base/experiments/refine"},"cor/refine"),":\nAn experimental extension of Roc with refinement of types bound in branch\npatterns. Provides a flow-typing-like ergonomics for a unification-based HM\nsystem."),(0,p.kt)("p",{parentName:"li"},"  Includes an compiler of pattern matching to decision trees, and various\nother optimizations."),(0,p.kt)("ul",{parentName:"li"},(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://ayazhafiz.com/plts/playground/cor/refine"},"Playground")))),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("p",{parentName:"li"},(0,p.kt)("a",{parentName:"p",href:"https://github.com/ayazhafiz/cor/tree/base/experiments/easy_tags"},"cor/easy_tags"),":\nAn experimental extension of Roc with polymorphic variants' type variables\nelided in output positions."),(0,p.kt)("ul",{parentName:"li"},(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://ayazhafiz.com/plts/playground/cor/easy_tags"},"Playground"))))),(0,p.kt)("h2",null,"Flow Typing"),(0,p.kt)("ul",null,(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("p",{parentName:"li"},(0,p.kt)("a",{parentName:"p",href:"https://ayazhafiz.com/lang_narrow"},"lang_narrow"),": A language with unions,\nrecords, and flow typing. A checker, interpreter, and C code generator is\nimplemented."),(0,p.kt)("ul",{parentName:"li"},(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://ayazhafiz.com/lang_narrow"},"Playground")),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://ayazhafiz.com/articles/21/lang-narrow"},"Blog post")))),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("p",{parentName:"li"},(0,p.kt)("a",{parentName:"p",href:"https://ayazhafiz.com/plts/playground/ft"},"FT"),": The FT (flow typing) calculus from David\nPearce's 2012 paper ",(0,p.kt)("a",{parentName:"p",href:"https://ecs.wgtn.ac.nz/foswiki/pub/Main/TechnicalReportSeries/ECSTR12-20.pdf"},(0,p.kt)("em",{parentName:"a"},"Sound and Complete Flow Typing with Unions,\nIntersections, and Negations")),".\nLike ",(0,p.kt)("inlineCode",{parentName:"p"},"lang_narrow"),", but smaller and proven sound and complete. Includes a\nself-designed type inferer guaranteed to infer principal types."),(0,p.kt)("ul",{parentName:"li"},(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://ayazhafiz.com/plts/playground/ft"},"Playground")),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://ecs.wgtn.ac.nz/foswiki/pub/Main/TechnicalReportSeries/ECSTR12-20.pdf"},"Pearce, 2012")),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://ayazhafiz.com/articles/21/type-inference-for-flow-typing"},"Blog post"),": type inference for the calculus")))),(0,p.kt)("h2",null,"Gradual Typing"),(0,p.kt)("ul",null,(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("p",{parentName:"li"},(0,p.kt)("a",{parentName:"p",href:"https://ayazhafiz.com/plts/playground/gtlc"},"gtlc"),": A compiler for the gradually-typed lambda calculus,\nemploying the type consistency relation of ",(0,p.kt)("a",{parentName:"p",href:"http://www.schemeworkshop.org/2006/13-siek.pdf"},"Siek and Taha")," (2006).\nThe GTLC allows a developer to omit type annotations during development at\nthe expense of run-time type casts. While the ahead-of-time typechecker will\ncatch any non-sensical type errors, the runtime system will catch any cast\nerrors."),(0,p.kt)("p",{parentName:"li"},"The compiler is multi-phase, optimizing, includes an interpretive mode and a\ntype inferer, and provides code generators to C and TypeScript."),(0,p.kt)("ul",{parentName:"li"},(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://ayazhafiz.com/plts/playground/gtlc"},"Playground")),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"http://www.schemeworkshop.org/2006/13-siek.pdf"},"Siek and Taha, 2006")),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.84.4703&rep=rep1&type=pdf"},"Type Inference for Gradual Typing (Siek and Vachharajani, 2008)"))))),(0,p.kt)("h2",null,"Typed Assembly"),(0,p.kt)("ul",null,(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://ayazhafiz.com/plts/playground/tal"},"TAL"),": A compiler from a System F-like language to the\nTyped Assembly Language of ",(0,p.kt)("a",{parentName:"li",href:"https://dash.harvard.edu/handle/1/2797451"},"Morrisett, et.al. 1998"),".\nAlso includes a compiler to x86 assembly using ",(0,p.kt)("a",{parentName:"li",href:"http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf"},"Linear Scan Register\nAllocation"),"\n(Poletto & Sarkar 1999).",(0,p.kt)("ul",{parentName:"li"},(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://ayazhafiz.com/plts/playground/tal"},"Playground"))))),(0,p.kt)("h2",null,"Subtyping"),(0,p.kt)("ul",null,(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("p",{parentName:"li"},(0,p.kt)("a",{parentName:"p",href:"https://ayazhafiz.com/plts/playground/ho21"},"HO21"),": An implementation of the\nalgorithmic duotyping calculus invented by Huang and Oliveira in\n",(0,p.kt)("a",{parentName:"p",href:"https://dl.acm.org/doi/pdf/10.1145/3473594"},"Distributing Intersection and Union Types with Splits and Duality")," (2021).\nThe calculus includes union, intersection, and arrow types in the presence\nof non-trivial distributivity rules. The authors' duotyping algorithm is\nsomewhat novel in that it computes subtyping relationship entirely on\nsurface types of the language, without normalizing to a form like DNF.\nThis implementation includes a type-derivation tree generator."),(0,p.kt)("ul",{parentName:"li"},(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://ayazhafiz.com/plts/playground/ho21"},"Playground")),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("a",{parentName:"li",href:"https://dl.acm.org/doi/pdf/10.1145/3473594"},"Huang and Oliveira, 2021")))),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("p",{parentName:"li"},(0,p.kt)("a",{parentName:"p",href:"https://github.com/ayazhafiz/plts/blob/base/simple_sub"},"simple_sub"),": A type system\nthat supports type inference in the presence of subtyping and polymorphism, as\ndescribed by Parreaux's ",(0,p.kt)("em",{parentName:"p"},(0,p.kt)("a",{parentName:"em",href:"https://lptk.github.io/files/%5Bv1.8%5D%20simple-essence-algebraic-subtyping.pdf"},"The Simple Essence of Algebraic Subtyping"))," (2020). Parreaux's\nwork distills Dolan's ",(0,p.kt)("a",{parentName:"p",href:"https://www.cs.tufts.edu/~nr/cs257/archive/stephen-dolan/thesis.pdf"},"2017 thesis"),"\non Algebraic Subtyping into a simpler core."))),(0,p.kt)("h2",null,"Dependent Types"),(0,p.kt)("ul",null,(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("p",{parentName:"li"},(0,p.kt)("a",{parentName:"p",href:"https://github.com/ayazhafiz/plts/blob/base/deptypes"},"deptypes"),": A dependent\ntype theory as described in Chapter 2 of Pierce's ",(0,p.kt)("em",{parentName:"p"},"Advanced Topics in Types\nand Programming Languages"),".")),(0,p.kt)("li",{parentName:"ul"},(0,p.kt)("p",{parentName:"li"},(0,p.kt)("a",{parentName:"p",href:"https://github.com/ayazhafiz/plts/pull/3"},"more deptypes"),": Additional,\nalternate implementations of the basic dependently-typed lambda calculus."))))}u.isMDXComponent=!0}}]);
//# sourceMappingURL=component---src-pages-index-mdx-3f6dc3d81b1c7c51fa34.js.map