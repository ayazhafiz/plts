# @plts/www

This package contains one-stop shop for adding a new playground to the `plts`
website.

Procedure:

- For a language `lang`, add a directory `lang/js`
    - `yarn init` in that directory and add `lang/js` as a workspace in the root
        `package.json`
    - Create a JavaScript target for `lang` (using js-of-ocaml). See `gtlc` for
        an example of how to do this.
    - Create a `.d.ts` file exposing the TypeScript interface of the JavaScript
        target.
- Add `lang` as a dependency of `www` and create a new `lang` playground in
    `src/pages/playground/lang.tsx`.
- Add a `lang` entry on the homepage `src/pages/index.tsx`.
- That's it! `yarn develop` to see the playground or publish it to deploy.

TODO: A script to automate these steps.
