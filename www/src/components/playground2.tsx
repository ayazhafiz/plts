import * as React from "react";
import type * as monaco from "monaco-editor";
import {
  Box,
  Heading,
  Details,
  useDetails,
  Popover,
  Link,
  Spinner,
  Button,
} from "@primer/react";
import styled from "styled-components";
import {space, SpaceProps} from "styled-system";
import * as lz from "lz-string";
import type {Backend, LanguageRegistration} from "../common/types";
import MdWrapper from "./md-wrapper";
import Editor from "@monaco-editor/react";
import {z} from "zod";

const ml = 3;

const Label = styled.label<SpaceProps>(space);
const Select = styled.select<SpaceProps>(space);

interface SelectorProps {
  options: string[];
  value: string | undefined;
  setValue: (value: string) => void;
  allowEmpty?: boolean;
}

function Selector({options, value, setValue, allowEmpty}: SelectorProps) {
  return (
    <Select
      ml={ml}
      onChange={(e) => setValue(e.target.value)}
      value={value ?? ""}
      className="form-select"
    >
      {allowEmpty && <option label=""></option>}
      {options.map((o) => (
        <option key={o} value={o}>
          {o}
        </option>
      ))}
    </Select>
  );
}

type Editor = monaco.editor.IStandaloneCodeEditor;

const EditorHeading: React.FC<{children: React.ReactNode}> = ({
  children,
}) => (
  <Box
    display="flex"
    flexDirection="row"
    alignItems="center"
    position="relative"
    width="100%"
  >
    {children}
  </Box>
);

const PopoverButton: React.FC<{heading: string; body: React.ReactNode}> = ({
  heading,
  body,
}) => {
  const {getDetailsProps} = useDetails({closeOnOutsideClick: true});

  return (
    <Box position="relative">
      <Details {...getDetailsProps()} sx={{ml: ml, mb: "0px !important"}}>
        <summary className="btn-link">{heading}</summary>
        <Popover open={true} caret="top-left">
          <Popover.Content sx={{mt: 2, pt: 3, pb: 0, width: "500px"}}>
            {body}
          </Popover.Content>
        </Popover>
      </Details>
    </Box>
  );
};

function initializeMonacoEditor(
  m: typeof monaco,
  languageRegistrations: Record<string, LanguageRegistration>,
) {
  m.editor.defineTheme("pgtheme", {
    base: "vs",
    inherit: true,
    colors: {},
    rules: [
      {token: "error", foreground: "ff0000"},
      {token: "infer", foreground: "ea5c00", fontStyle: "italic"},
    ],
  });

  for (const [lang, {syntax, hover, format, autoFormat}] of Object.entries(
    languageRegistrations,
  )) {
    m.languages.register({id: lang});
    m.languages.setMonarchTokensProvider(lang, syntax);
    if (hover) {
      m.languages.registerHoverProvider(lang, {
        provideHover: hover(m),
      });
    }
    if (format) {
      m.languages.registerDocumentFormattingEditProvider(lang, {
        provideDocumentFormattingEdits: format,
      });
    }
    if (autoFormat) {
      m.languages.registerOnTypeFormattingEditProvider(lang, {
        provideOnTypeFormattingEdits: autoFormat.format,
        autoFormatTriggerCharacters: autoFormat.triggerCharacters,
      });
    }
  }
}

function CodeEditor({
  text,
  setText,
  languageRegistrations,
  readOnly,
  language,
}: {
  text: string;
  setText: (text: string) => void;
  languageRegistrations: Record<string, LanguageRegistration>;
  readOnly?: boolean;
  language: string;
}) {
  return (
    <Editor
      value={text}
      onChange={(value) => {
        setText(value ?? "");
      }}
      beforeMount={(m) => initializeMonacoEditor(m, languageRegistrations)}
      theme="pgtheme"
      language={language}
      options={{
        fontSize: 15,
        automaticLayout: true,
        padding: {},
        formatOnType: true,
        formatOnPaste: true,
        autoIndent: "full",
        readOnly,
      }}
    ></Editor>
  );
}

function InputColumn({
  source,
  grammar,
  examples,
  defaultExample,
  text,
  setText,
  languageRegistrations,
  language,
}: {
  source: string;
  grammar: React.ReactNode | string;
  examples: Record<string, string>;
  defaultExample: string;
  text: string;
  setText: (text: string) => void;
  languageRegistrations: Record<string, LanguageRegistration>;
  language: string;
}) {
  const [example, setExample] = React.useState<string | undefined>(
    defaultExample,
  );

  React.useEffect(() => {
    for (const [example, t] of Object.entries(examples)) {
      if (text === t) {
        setExample(example);
        return;
      }
    }
    setExample(undefined);
  }, [text, examples]);

  return (
    <>
      <Box
        display="flex"
        flexDirection="row"
        alignItems="center"
        justifyContent="space-between"
      >
        <EditorHeading>
          <Heading as="h2" sx={{display: "inline-block"}}>
            Input
          </Heading>
          <Selector
            allowEmpty
            options={Object.keys(examples)}
            value={example}
            setValue={(example) => {
              setExample(example);
              setText(examples[example]);
            }}
          />
          {typeof grammar === "string" ? (
            <Link sx={{ml}} href={grammar}>
              Language Grammar
            </Link>
          ) : (
            <PopoverButton heading="Language Grammar" body={grammar} />
          )}
          <Link sx={{ml}} href={source}>
            Source
          </Link>
        </EditorHeading>
      </Box>
      <CodeEditor
        text={text}
        setText={setText}
        languageRegistrations={languageRegistrations}
        language={language}
      ></CodeEditor>
    </>
  );
}

interface BackendBlockProps<Backends extends Record<string, Backend>> {
  text: string;
  backends: Backends;
  backend: string;
  emits: string[];
  emit: string;
  languageRegistrations: Record<string, LanguageRegistration>;
  onRemove: () => void;
  setBackend: (x: string) => void;
  setEmit: (x: string) => void;
}

function BackendBlock<Backends extends Record<string, Backend>>({
  text,
  backends,
  emits,
  languageRegistrations,
  onRemove,
  backend,
  emit,
  setBackend,
  setEmit,
}: BackendBlockProps<Backends>) {
  const [compiled, setCompiled] = React.useState("");
  const [error, setError] = React.useState<string | null>(null);
  const [isCompiling, setIsCompiling] = React.useState(false);

  React.useEffect(() => {
    async function compile() {
      const compiler = backends[backend].do;
      setIsCompiling(true);
      try {
        const result = await compiler(text, emit);
        if (result.error !== null) {
          setError(result.error);
        } else {
          setCompiled(result.result ?? "");
          setError(null);
        }
      } finally {
        setIsCompiling(false);
      }
    }
    compile();
  }, [backend, emit, text]);

  return (
    <>
      <Box
        display="flex"
        flexDirection="row"
        alignItems="center"
        justifyContent="space-between"
      >
        <EditorHeading>
          <Box
            display="flex"
            flexDirection="row"
            alignItems="center"
            justifyContent="space-between"
            width="100%"
          >
            <Box
              display="flex"
              flexDirection="row"
              alignItems="center"
              position="relative"
            >
              <Heading as="h2" sx={{display: "inline-block"}}>
                Output
              </Heading>
              <Box
                display="flex"
                flexDirection="row"
                alignItems="center"
                sx={{ml}}
              >
                <Label>Backend</Label>
                <Selector
                  options={Object.keys(backends)}
                  value={backend}
                  setValue={(example) => setBackend(example)}
                />
              </Box>
              <Box
                display="flex"
                flexDirection="row"
                alignItems="center"
                sx={{ml}}
              >
                <Label>Emit</Label>
                <Selector
                  options={emits}
                  value={emit}
                  setValue={(example) => setEmit(example)}
                />
              </Box>
            </Box>
            <Box>
              <Button
                variant="danger"
                type="button"
                onClick={onRemove}
                sx={{
                  width: "fit-content",
                }}
              >
                Remove
              </Button>
            </Box>
          </Box>
        </EditorHeading>
      </Box>
      {isCompiling && <Spinner size="medium" sx={{ml}} />}
      <CodeEditor
        text={error ?? compiled}
        setText={() => {}}
        languageRegistrations={languageRegistrations}
        language="text"
        readOnly
      ></CodeEditor>
    </>
  );
}

function withWindow<T>(f: (window: Window) => T): T | null {
  if (typeof window !== undefined) {
    return f(window);
  }
  return null;
}

function commitPersistentState(state: PersistentState) {
  withWindow((window: Window) => {
    const queryParams = new URLSearchParams(window.location.search);
    queryParams.set(
      "state",
      lz.compressToEncodedURIComponent(JSON.stringify(state)),
    );
    const curUrl = `${window.location.pathname}?${queryParams}`;
    history.replaceState(null, "", curUrl);
  });
}

function getStateStringFromUrl() {
  return withWindow((window: Window) => {
    const queryParams = new URLSearchParams(window.location.search);
    return queryParams.get("state");
  });
}

function parseStateString(input: string | null) {
  if (input === null) {
    return null;
  }
  const decompressed = lz.decompressFromEncodedURIComponent(input);
  try {
    const state = JSON.parse(decompressed);
    return TPersistentState.parse(state);
  } catch (e) {
    console.error(e);
    return null;
  }
}

interface PlaygroundProps<
  Backends extends Record<string, Backend>,
  Examples extends Record<string, string>,
> {
  title: string;
  language: string;
  source: string;
  grammar: React.ReactNode | string;
  languageRegistrations: Record<string, LanguageRegistration>;
  backends: Backends;
  defaultBackend: keyof Backends & string;
  emits: string[];
  defaultEmit: string;
  examples: Examples;
  defaultExample: keyof Examples & string;
}

const TBackendId = z.object({
  backend: z.string(),
  emit: z.string(),
});
type BackendId = z.infer<typeof TBackendId>;

const TPersistentState = z.object({
  input: z.string(),
  backends: z.array(TBackendId),
});
type PersistentState = z.infer<typeof TPersistentState>;

function Playground<
  Backends extends Record<string, Backend>,
  Examples extends Record<string, string>,
>(props: PlaygroundProps<Backends, Examples>) {
  const stateString = getStateStringFromUrl();

  const [text, setText] = React.useState<string>(
    props.examples[props.defaultExample] ?? "",
  );

  const defaultBackendId = {
    backend: props.defaultBackend,
    emit: props.defaultEmit,
  };

  const [backendIds, setBackendIds] = React.useState<BackendId[]>([
    {...defaultBackendId},
  ]);

  function addBackend() {
    setBackendIds([...backendIds, {...defaultBackendId}]);
  }

  function removeBackend(i: number) {
    setBackendIds([...backendIds.slice(0, i), ...backendIds.slice(i + 1)]);
  }

  function updateBackend(i: number, {backend, emit}: BackendId) {
    setBackendIds((backendIds) => [
      ...backendIds.slice(0, i),
      {...backendIds[i], backend, emit},
      ...backendIds.slice(i + 1),
    ]);
  }

  React.useEffect(() => {
    const state = parseStateString(stateString);
    if (state !== null) {
      console.log("setting state", state);
      setText(state.input);
      setBackendIds(state.backends);
    }
  }, []);

  React.useEffect(() => {
    commitPersistentState({
      input: text,
      backends: backendIds,
    });
  }, [text, backendIds]);

  return (
    <MdWrapper title={props.title} margin={[0, 0, 0]}>
      <Box display="grid" gridTemplateColumns="1fr 1fr" gridGap={0}>
        <Box
          display="flex"
          flex={1}
          flexDirection="column"
          pt="0px"
          pb="24px"
          px="20px"
          height="100vh"
          margin={0}
        >
          <InputColumn
            source={props.source}
            grammar={props.grammar}
            examples={props.examples}
            defaultExample={props.defaultExample}
            text={text}
            setText={setText}
            languageRegistrations={props.languageRegistrations}
            language={props.language}
          ></InputColumn>
        </Box>

        <Box
          display="flex"
          flex={1}
          flexDirection="column"
          pt="0px"
          pb="24px"
          px="20px"
          height="100vh"
          margin={0}
        >
          {backendIds.map(({backend, emit}, i) => (
            <Box
              key={i}
              style={{maxHeight: `${100 / i}%`, overflow: "hidden"}}
            >
              <BackendBlock
                text={text}
                backends={props.backends}
                onRemove={() => removeBackend(i)}
                backend={backend}
                emit={emit}
                emits={props.emits}
                languageRegistrations={props.languageRegistrations}
                setBackend={(backend) => updateBackend(i, {backend, emit})}
                setEmit={(emit) => updateBackend(i, {backend, emit})}
              ></BackendBlock>
            </Box>
          ))}

          <Button
            variant="primary"
            type="button"
            onClick={addBackend}
            sx={{
              width: "fit-content",
            }}
          >
            Add backend
          </Button>
        </Box>
      </Box>
    </MdWrapper>
  );
}

export default Playground;
