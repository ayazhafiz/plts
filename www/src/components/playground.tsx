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
  TextInput,
} from "@primer/react";
import styled from "styled-components";
import { space, SpaceProps } from "styled-system";
import * as lz from "lz-string";
import type {
  Result,
  Backend,
  BackendKind,
  LanguageRegistration,
  StringOptions,
  BackendOptions,
} from "../common/types";
import MdWrapper from "./md-wrapper";
import Revision from "./revision";

const ml = 3;

const PgColumn: React.FC<{ children: React.ReactNode }> = (props) => (
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
    {props.children}
  </Box>
);

const Label = styled.label<SpaceProps>(space);
const Select = styled.select<SpaceProps>(space);
const Span = styled.span<SpaceProps>(space);

type ForceSetValue = (forceSet: (option: string) => Promise<void>) => void;

interface SelectorProps {
  options: string[];
  defaultOption: string;
  onChange: (option: string) => void;
  forceSetValue: ForceSetValue;
}

class Selector extends React.Component<SelectorProps, { value: string }> {
  constructor(props: SelectorProps) {
    super(props);
    this.state = { value: props.defaultOption };
    props.forceSetValue((option) => {
      this.setState({ value: option });
      return Promise.resolve();
    });
  }

  onChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    this.props.onChange(e.target.value);
    this.setState({ value: e.target.value });
  };

  override render() {
    return (
      <Select
        ml={ml}
        onChange={this.onChange}
        value={this.state.value}
        className="form-select"
      >
        {this.props.options.map((o) => (
          <option key={o} value={o}>
            {o}
          </option>
        ))}
      </Select>
    );
  }
}

type Editor = monaco.editor.IStandaloneCodeEditor;

const EditorHeading: React.FC<{ children: React.ReactNode }> = ({
  children,
}) => (
  <Box
    display="flex"
    flexDirection="row"
    alignItems="center"
    position="relative"
  >
    {children}
  </Box>
);

const PopoverButton: React.FC<{ heading: string; body: React.ReactNode }> = ({
  heading,
  body,
}) => {
  const { getDetailsProps } = useDetails({ closeOnOutsideClick: true });

  return (
    <Box position="relative">
      <Details {...getDetailsProps()} sx={{ ml: ml, mb: "0px !important" }}>
        <summary className="btn-link">{heading}</summary>
        <Popover open={true} caret="top-left">
          <Popover.Content sx={{ mt: 2, pt: 3, pb: 0, width: "500px" }}>
            {body}
          </Popover.Content>
        </Popover>
      </Details>
    </Box>
  );
};

type OnDidBackendChange = (subscriber: () => Promise<"done">) => void;
type OnDidInputChange = (
  subscriber: (newInput: string) => Promise<"done">
) => void;

const InputColumn = ({
  source,
  grammar,
  examples,
  defaultExample,
  backendOptions,
  defaultBackend,
  setBackend,
  forceSetBackend,
  getEditor,
  children,
}: {
  source: string;
  grammar: React.ReactNode | string;
  examples: Record<string, string>;
  defaultExample: string;
  backendOptions: string[];
  defaultBackend: string;
  setBackend: (newBackend: string) => void;
  forceSetBackend: ForceSetValue;
  getEditor: () => Promise<Editor>;
  children: React.ReactNode;
}) => {
  const setExample = async (choice: string) => {
    (await getEditor()).setValue(examples[choice]);
  };
  return (
    <PgColumn>
      <Box
        display="flex"
        flexDirection="row"
        alignItems="center"
        justifyContent="space-between"
      >
        <EditorHeading>
          <Heading as="h1" sx={{ display: "inline-block" }}>
            Input
          </Heading>
          <Selector
            options={Object.keys(examples)}
            defaultOption={defaultExample}
            onChange={setExample}
            forceSetValue={(_it: any) => {}}
          />
          {typeof grammar === "string" ? (
            <Link sx={{ ml }} href={grammar}>
              Language Grammar
            </Link>
          ) : (
            <PopoverButton heading="Language Grammar" body={grammar} />
          )}
          <Link sx={{ ml }} href={source}>
            Source
          </Link>
        </EditorHeading>
        <Selector
          options={backendOptions}
          defaultOption={defaultBackend}
          onChange={setBackend}
          forceSetValue={forceSetBackend}
        />
      </Box>
      {children}
    </PgColumn>
  );
};

type SetHide = (hide: boolean) => Promise<void>;

interface BackendBlockProps {
  identity: number;
  prio: number;
  getMonaco: () => typeof monaco;
  getBackend: () => Backend | null;
  getEditor: () => Promise<Editor>;
  onDidBackendChange: OnDidBackendChange;
  onDidInputChange: OnDidInputChange;
  registerEditor: (setHide: SetHide) => void;
  children?: React.ReactNode;
}

class BackendBlock extends React.Component<
  BackendBlockProps,
  {
    title: string | null;
    options: BackendOptions | null;
    info: [string, React.ReactNode][];
    result: "loading" | Result | null;
    forceHide: boolean;
  }
> {
  private readonly nullState = {
    title: null,
    options: null,
    info: [],
    result: null,
    forceHide: false,
  };
  private lastKnownInput = "";

  constructor(props: BackendBlockProps) {
    super(props);

    const { getBackend, onDidBackendChange, onDidInputChange, registerEditor } =
      this.props;

    registerEditor(this.setHide);

    const backend = getBackend();
    if (backend === null) {
      this.state = this.nullState;
    } else {
      this.state = {
        title: backend.title,
        options: backend.options,
        info: backend.info ? backend.info : [],
        result: { result: "", error: null },
        forceHide: false,
      };
    }

    onDidBackendChange(this.updateBackend);
    onDidInputChange(this.updateOutput);
  }

  setStateAsync = (newState: any) =>
    new Promise<void>((resolve) => this.setState(newState, resolve));

  setHide = async (forceHide: boolean) => {
    await this.setStateAsync({ forceHide });
  };

  updateBackend = async (): Promise<"done"> => {
    const { getBackend, getMonaco, getEditor } = this.props;
    const backend = getBackend();
    if (backend === null) {
      await this.setStateAsync(this.nullState);
      return "done";
    }
    const monaco = getMonaco();
    await this.setStateAsync({
      title: backend.title,
      options: backend.options,
      info: backend.info ? backend.info : [],
    });
    monaco.editor.setModelLanguage(
      (await getEditor()).getModel()!,
      backend.editorLanguage
    );
    return this.updateOutput();
  };

  updateOutput = async (
    input: string = this.lastKnownInput
  ): Promise<"done"> => {
    this.lastKnownInput = input;
    const { getBackend, getEditor } = this.props;
    const backend = getBackend();
    const { options } = this.state;
    if (backend === null || options === null) return "done";

    const ed = await getEditor();

    await this.setStateAsync({ result: "loading" });
    ed.setValue("");

    const optionValues = options.map(([_, v]) => {
      if (typeof v === "object") {
        return v.value;
      }
      return v;
    });

    const result = await backend.do(input, ...optionValues);
    if (result.result !== null) {
      ed.setValue(result.result);
      ed.trigger("playground", "editor.foldAllMarkerRegions", {});
    }
    await this.setStateAsync({ result });
    return "done";
  };

  setOption = async (e: { checked: boolean; value: string }, i: number) => {
    let v = this.state.options![i][1];
    switch (typeof v) {
      case "boolean": {
        v = e.checked;
        break;
      }
      case "number": {
        v = parseInt(e.value) || 0;
        break;
      }
      case "object": {
        v.value = e.value;
        break;
      }
    }
    this.state.options![i][1] = v;
    await this.setStateAsync({ options: this.state.options });
    writePersistentBackendOption(this.props.identity, this.state.options!);
    this.updateOutput();
  };

  override render() {
    const { result, title, options, info, forceHide } = this.state;
    const globalHide = forceHide || result === null;
    const isLoading = result === "loading";
    const hideError = globalHide || isLoading || result.error === null;
    const hideResult = globalHide || (!isLoading && result.result === null);
    const error = hideError ? "" : result.error;
    const titleTxt = title ?? "";
    const optionsLst = options ?? [];
    const createOptionsHtml = (
      opt: string,
      val: boolean | number | StringOptions,
      i: number
    ) => {
      switch (typeof val) {
        case "boolean": {
          return (
            <>
              <input
                id={opt}
                type="checkbox"
                checked={val}
                onChange={(e) => this.setOption(e.target, i)}
              />
              <Label htmlFor={opt} ml={2}>
                {opt}
              </Label>
            </>
          );
        }
        case "number": {
          return (
            <>
              <Label htmlFor={opt}>{opt}</Label>
              <TextInput
                sx={{ ml: 2, p: 0, px: 1 }}
                id={opt}
                type="number"
                min={0}
                max={120}
                value={val}
                onChange={(e: React.ChangeEvent<HTMLInputElement>) =>
                  this.setOption(e.target, i)
                }
              />
            </>
          );
        }
        case "object": {
          return (
            <>
              <Label htmlFor={opt}>{opt}</Label>
              <Selector
                options={val.options}
                defaultOption={val.value}
                onChange={(value: string) =>
                  this.setOption({ checked: false, value }, i)
                }
                forceSetValue={(_it) => {}}
              />
            </>
          );
        }
        default:
          throw new Error(`unknown option type ${JSON.stringify(val)}`);
      }
    };
    return (
      <Box
        style={{ display: globalHide ? "none" : "flex" }}
        flex={this.props.prio}
        flexDirection="column"
      >
        <EditorHeading>
          <Heading as="h1" sx={{ display: "inline-block" }}>
            {titleTxt}
          </Heading>
          {optionsLst.map(([opt, val], i) => (
            <Span key={i} ml={ml}>
              {createOptionsHtml(opt, val, i)}
            </Span>
          ))}
          {info.map(([title, content], i) => (
            <PopoverButton key={i} heading={title} body={content} />
          ))}
          {this.props.identity === 0 ? (
            <Span marginLeft={"auto"} marginRight={1}>
              <Revision />
            </Span>
          ) : (
            <></>
          )}
        </EditorHeading>

        {/* Error */}
        <Box display={hideError ? "none" : "block"}>
          <pre>{error}</pre>
        </Box>

        {/* Result */}
        <Box
          display={hideResult ? "none" : "flex"}
          flexDirection="column"
          flex="1"
        >
          {isLoading ? <Spinner size="medium" sx={{ ml }} /> : <></>}
          <Box display={isLoading ? "none" : "flex"} flex="1">
            {this.props.children}
          </Box>
        </Box>
      </Box>
    );
  }
}

const editorOpts: monaco.editor.IStandaloneEditorConstructionOptions = {
  theme: "vs",
  fontSize: 15,
  padding: {},
  automaticLayout: true,
  formatOnType: true,
  formatOnPaste: true,
  autoIndent: "full",
  value: "",
};

interface PlaygroundProps<
  Backends extends Record<string, BackendKind>,
  Examples extends Record<string, string>
> {
  title: string;
  language: string;
  source: string;
  grammar: React.ReactNode | string;
  languageRegistrations: Record<string, LanguageRegistration>;
  backends: Backends;
  defaultBackend: keyof Backends & string;
  examples: Examples;
  defaultExample: keyof Examples & string;
}

interface PersistentState {
  input: string;
  backend: string;
  options: BackendOptions[] | null;
}

let persistentState: PersistentState;

function writePersistentBackendOption(
  backendIdentity: number,
  options: BackendOptions
) {
  persistentState.options![backendIdentity] = options;
  commitPersistentState();
}

function writePersistentState<K extends keyof PersistentState>(
  key: K,
  value: PersistentState[K]
) {
  persistentState[key] = value;
  commitPersistentState();
}

function withWindow(f: (window: Window) => void) {
  if (typeof window !== undefined) {
    f(window);
  }
}

function commitPersistentState() {
  withWindow((window: Window) => {
    const queryParams = new URLSearchParams(window.location.search);
    queryParams.set(
      "input",
      lz.compressToEncodedURIComponent(persistentState.input)
    );
    queryParams.set("backend", persistentState.backend);
    if (persistentState.options !== null) {
      queryParams.set(
        "options",
        lz.compressToEncodedURIComponent(
          JSON.stringify(persistentState.options)
        )
      );
    }
    const curUrl = `${window.location.pathname}?${queryParams}`;
    history.replaceState(null, "", curUrl);
  });
}

function loadPersistentState({
  defaultInput,
  defaultBackend,
}: {
  defaultInput: string;
  defaultBackend: string;
}) {
  withWindow((window: Window) => {
    const queryParams = new URLSearchParams(window.location.search);
    const input = queryParams.get("input")
      ? lz.decompressFromEncodedURIComponent(queryParams.get("input")!)!
      : defaultInput;
    console.info("default backend", defaultBackend);
    const backend = queryParams.get("backend") ?? defaultBackend;
    const options = queryParams.get("options")
      ? JSON.parse(
          lz.decompressFromEncodedURIComponent(queryParams.get("options")!)!
        )
      : null;
    persistentState = {
      input,
      backend,
      options,
    };
  });
}

class EditorCell {
  private readonly resolve: (ed: Editor) => void;
  private readonly editor: Promise<Editor>

  constructor() {
    let theResolve: (ed: Editor) => void = null!;
    this.editor = new Promise((resolve, _reject) => {
      theResolve = resolve;
    });
    console.assert(theResolve !== null);
    this.resolve = theResolve;
  }

  set(ed: Editor) {
    this.resolve(ed);
  }

  async get() {
    return await this.editor;
  }
}

class Playground<
  Backends extends Record<string, BackendKind>,
  Examples extends Record<string, string>
> extends React.Component<PlaygroundProps<Backends, Examples>> {
  private readonly editors: Record<
    string,
    { kind: "input" | "output"; editor: EditorCell; setHide?: SetHide }
  > = {};
  private inputEditorId: string = "input-editor";

  private backend: BackendKind = this.props.backends[this.props.defaultBackend];
  private readonly backendChangeSubscribers: Array<() => Promise<"done">> = [];
  private readonly inputChangeSubscribers: Array<
    (input: string) => Promise<"done">
  > = [];

  private forceSetBackend!: (option: string) => Promise<void>;

  private monaco!: typeof monaco;

  constructor(props: PlaygroundProps<Backends, Examples>) {
    super(props);
  }

  getMonaco = () => this.monaco;

  registerEditor = (
    editorId: string,
    kind: "input" | "output",
    setHide?: SetHide
  ) => {
    this.editors[editorId] = {
      kind,
      editor: new EditorCell(), // will get updated during componentDidMount
      setHide,
    };
  };

  getEditor = async (editorId: string) => {
    return await this.editors[editorId].editor.get();
  };

  getBackend = () => this.backend;

  setBackend = async (newBackend: keyof Backends & string) => {
    writePersistentState("backend", newBackend);
    this.backend = this.props.backends[newBackend];
    return this.backendChange();
  };

  inputChange = async () => {
    const newInput = (await this.getEditor(this.inputEditorId)).getValue();
    writePersistentState("input", newInput);
    console.debug("firing new input of length ", newInput.length);
    return Promise.all(this.inputChangeSubscribers.map((s) => s(newInput)));
  };

  backendChange = async () => {
    await Promise.all(this.backendChangeSubscribers.map((s) => s()));
    await this.resizeAllEditors();
  };

  resizeAllEditors = async () => {
    // HACK: Editors do not auto-resize when we switch from one backend to two,
    // but flipping the hide switches off and on seems to do the trick.
    const hideSwitches = Object.values(this.editors)
      .map((e) => e.setHide)
      .filter((sh): sh is SetHide => sh !== undefined);
    await Promise.allSettled(hideSwitches.map((setHide) => setHide(true)));
    await new Promise((resolve) => setTimeout(resolve, 1)); // HACK
    await Promise.allSettled(hideSwitches.map((setHide) => setHide(false)));
  };

  onDidBackendChange = (subscriber: () => Promise<"done">) => {
    this.backendChangeSubscribers.push(subscriber);
  };

  onDidInputChange = (subscriber: (newInput: string) => Promise<"done">) => {
    this.inputChangeSubscribers.push(subscriber);
  };

  override async componentDidMount() {
    import("monaco-editor").then(async (monaco) => {
      this.monaco = monaco;

      monaco.editor.defineTheme("pgtheme", {
        base: "vs",
        inherit: true,
        colors: {},
        rules: [
          { token: "error", foreground: "ff0000" },
          { token: "infer", foreground: "ea5c00", fontStyle: "italic" },
        ],
      });

      for (const [
        lang,
        { syntax, hover, format, autoFormat },
      ] of Object.entries(this.props.languageRegistrations)) {
        monaco.languages.register({ id: lang });
        monaco.languages.setMonarchTokensProvider(lang, syntax);
        if (hover) {
          monaco.languages.registerHoverProvider(lang, {
            provideHover: hover(monaco),
          });
        }
        if (format) {
          monaco.languages.registerDocumentFormattingEditProvider(lang, {
            provideDocumentFormattingEdits: format,
          });
        }
        if (autoFormat) {
          monaco.languages.registerOnTypeFormattingEditProvider(lang, {
            provideOnTypeFormattingEdits: autoFormat.format,
            autoFormatTriggerCharacters: autoFormat.triggerCharacters,
          });
        }
      }

      loadPersistentState({
        defaultInput: this.props.examples[this.props.defaultExample],
        defaultBackend: this.props.defaultBackend,
      });

      for (const editorId of Object.keys(this.editors)) {
        const extraOpts: monaco.editor.IStandaloneEditorConstructionOptions =
          this.editors[editorId].kind === "output"
            ? { readOnly: true }
            : { language: this.props.language };
        const editor = monaco.editor.create(
          document.getElementById(editorId)!,
          {
            ...editorOpts,
            theme: "pgtheme",
            ...extraOpts,
          }
        );
        this.editors[editorId].editor.set(editor);

        if (this.editors[editorId].kind === "input") {
          editor.setValue(persistentState.input);
          console.info("set initial value of length ", persistentState.input.length);
          editor.onDidChangeModelContent(this.inputChange);
        }
      }

      this.backend = this.props.backends[persistentState.backend];
      if (persistentState.options !== null) {
        for (let i = 0; i < this.backend.length; ++i) {
          this.backend[i].options = persistentState.options[i];
        }
      }
      await this.forceSetBackend(persistentState.backend);

      writePersistentState(
        "options",
        this.backend.map((back) => back.options)
      );

      await this.backendChange();
      await this.inputChange();
    });
  }

  override render() {
    const backendEditors = ["backend-editor1", "backend-editor2"];
    const getBackendAt = (index: number) => {
      const backends = this.getBackend();
      if (index < backends.length) return backends[index];
      return null;
    };
    const getBackendEditorAt = async (index: number) =>
      await this.getEditor(backendEditors[index]);
    const getInputEditor = () => this.getEditor(this.inputEditorId);

    this.registerEditor(this.inputEditorId, "input");

    return (
      <MdWrapper title={this.props.title} margin={[0, 0, 0]}>
        <Box display="grid" gridTemplateColumns="1fr 1fr" gridGap={0}>
          <InputColumn
            source={this.props.source}
            grammar={this.props.grammar}
            examples={this.props.examples}
            defaultExample={this.props.defaultExample}
            backendOptions={Object.keys(this.props.backends)}
            defaultBackend={this.props.defaultBackend}
            forceSetBackend={(setIt) => {
              this.forceSetBackend = setIt;
            }}
            setBackend={this.setBackend}
            getEditor={getInputEditor}
          >
            <Box id={this.inputEditorId} flex={1}></Box>
          </InputColumn>

          {/* Output */}
          <PgColumn>
            {[0, 1].map((i) => (
              <BackendBlock
                key={i}
                identity={i}
                prio={[2, 1][i]}
                getMonaco={this.getMonaco}
                getBackend={() => getBackendAt(i)}
                getEditor={() => getBackendEditorAt(i)}
                onDidBackendChange={this.onDidBackendChange}
                onDidInputChange={this.onDidInputChange}
                // TODO: this is really ugly
                registerEditor={(setHide: SetHide) => {
                  this.registerEditor(backendEditors[i], "output", setHide);
                }}
              >
                <Box id={backendEditors[i]} flex="1"></Box>
              </BackendBlock>
            ))}
          </PgColumn>
        </Box>
      </MdWrapper>
    );
  }
}

export default Playground;
