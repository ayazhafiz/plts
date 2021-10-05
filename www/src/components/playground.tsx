import * as React from "react";
import type * as monaco from "monaco-editor";
import Box from "@primer/components/lib/Box";
import Heading from "@primer/components/lib/Heading";
import Details from "@primer/components/lib/Details";
import useDetails from "@primer/components/lib/hooks/useDetails";
import Popover from "@primer/components/lib/Popover";
import Link from "@primer/components/lib/Link";
import Spinner from "@primer/components/lib/Spinner";
import styled from "styled-components";
import { space, SpaceProps } from "styled-system";
import type { Result, Backend, BackendKind, LanguageRegistration } from "../common/types";
import MdWrapper from "./md-wrapper";

const snd = <S, T>([_, s]: [S, T]) => s;

const ml = 3;

const PgColumn: React.FC<{}> = (props) => (
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

interface SelectorProps {
  options: string[];
  defaultOption: string;
  onChange: (option: string) => void;
}

class Selector extends React.Component<SelectorProps, {}> {
  constructor(props: SelectorProps) {
    super(props);
  }

  onChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    this.props.onChange(e.target.value);
  };

  override render() {
    return (
      <Select
        ml={ml}
        onChange={this.onChange}
        defaultValue={this.props.defaultOption}
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

const EditorHeading: React.FC<{}> = (props) => (
  <Box display="flex" flexDirection="row" alignItems="center" position="relative">
    {props.children}
  </Box>
);

const PopoverButton: React.FC<{ heading: string; body: React.ReactNode }> = ({ heading, body }) => {
  const { getDetailsProps } = useDetails({ closeOnOutsideClick: true });

  return (
    <Box position="relative">
      <Details {...getDetailsProps()} ml={ml} mb="0px !important">
        <summary className="btn-link">{heading}</summary>
        <Popover open={true} caret="top-left">
          <Popover.Content sx={{ mt: 2, pt: 3, pb: 0, width: "500px" }}>{body}</Popover.Content>
        </Popover>
      </Details>
    </Box>
  );
};

type OnDidBackendChange = (subscriber: () => Promise<"done">) => void;
type OnDidInputChange = (subscriber: (newInput: string) => Promise<"done">) => void;

const InputColumn = ({
  source,
  grammar,
  examples,
  defaultExample,
  backendOptions,
  defaultBackend,
  setBackend,
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
  getEditor: () => Editor;
  children: React.ReactNode;
}) => {
  const setExample = (choice: string) => {
    getEditor().setValue(examples[choice]);
  };
  return (
    <PgColumn>
      <Box display="flex" flexDirection="row" alignItems="center" justifyContent="space-between">
        <EditorHeading>
          <Heading as="h1" display="inline-block">
            Input
          </Heading>
          <Selector
            options={Object.keys(examples)}
            defaultOption={defaultExample}
            onChange={setExample}
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
        <Selector options={backendOptions} defaultOption={defaultBackend} onChange={setBackend} />
      </Box>
      {children}
    </PgColumn>
  );
};

type SetHide = (hide: boolean) => Promise<void>;

interface BackendBlockProps {
  prio: number;
  getMonaco: () => typeof monaco;
  getBackend: () => Backend | null;
  getEditor: () => Editor;
  onDidBackendChange: OnDidBackendChange;
  onDidInputChange: OnDidInputChange;
  registerEditor: (setHide: SetHide) => void;
}

class BackendBlock extends React.Component<
  BackendBlockProps,
  {
    title: string | null;
    options: [string, boolean][] | null;
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

    const { getBackend, onDidBackendChange, onDidInputChange, registerEditor } = this.props;

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
    monaco.editor.setModelLanguage(getEditor().getModel()!, backend.editorLanguage);
    return this.updateOutput();
  };

  updateOutput = async (input: string = this.lastKnownInput): Promise<"done"> => {
    const { getBackend, getEditor } = this.props;
    const backend = getBackend();
    const { options } = this.state;
    if (backend === null || options === null) return "done";
    this.lastKnownInput = input;

    await this.setStateAsync({ result: "loading" });
    getEditor().setValue("");

    const result = await backend.do(input, ...options.map(snd));
    if (result.result !== null) {
      getEditor().setValue(result.result);
      getEditor().trigger("playground", "editor.foldAllMarkerRegions", {});
    }
    await this.setStateAsync({ result });
    return "done";
  };

  setArg = async (e: React.ChangeEvent<HTMLInputElement>, i: number) => {
    this.state.options![i][1] = e.target.checked;
    await this.setStateAsync({ options: this.state.options });
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
    return (
      <Box
        style={{ display: globalHide ? "none" : "flex" }}
        flex={this.props.prio}
        flexDirection="column"
      >
        <EditorHeading>
          <Heading as="h1" display="inline-block">
            {titleTxt}
          </Heading>
          {optionsLst.map(([opt, flag], i) => (
            <Span key={i} ml={ml}>
              <input id={opt} type="checkbox" checked={flag} onChange={(e) => this.setArg(e, i)} />
              <Label htmlFor={opt} ml={2}>
                {opt}
              </Label>
            </Span>
          ))}
          {info.map(([title, content], i) => (
            <PopoverButton key={i} heading={title} body={content} />
          ))}
        </EditorHeading>

        {/* Error */}
        <Box display={hideError ? "none" : "block"}>
          <pre>{error}</pre>
        </Box>

        {/* Result */}
        <Box display={hideResult ? "none" : "flex"} flexDirection="column" flex="1">
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
  language: string;
  source: string;
  grammar: React.ReactNode | string;
  languageRegistrations: Record<string, LanguageRegistration>;
  backends: Backends;
  defaultBackend: keyof Backends & string;
  examples: Examples;
  defaultExample: keyof Examples & string;
}

class Playground<
  Backends extends Record<string, BackendKind>,
  Examples extends Record<string, string>
> extends React.Component<PlaygroundProps<Backends, Examples>> {
  private readonly editors: Record<
    string,
    { kind: "input" | "output"; editor: Editor; setHide?: SetHide }
  > = {};
  private inputEditorId: string = "input-editor";

  private backend = this.props.backends[this.props.defaultBackend];
  private readonly backendChangeSubscribers: Array<() => Promise<"done">> = [];
  private readonly inputChangeSubscribers: Array<(input: string) => Promise<"done">> = [];

  private monaco!: typeof monaco;

  constructor(props: PlaygroundProps<Backends, Examples>) {
    super(props);
  }

  getMonaco = () => this.monaco;

  registerEditor = (editorId: string, kind: "input" | "output", setHide?: SetHide) => {
    this.editors[editorId] = {
      kind,
      editor: null!, // will get updated during componentDidMount
      setHide,
    };
  };

  getEditor = (editorId: string) => {
    return this.editors[editorId].editor;
  };

  getBackend = () => this.backend;

  setBackend = (newBackend: keyof Backends & string) => {
    this.backend = this.props.backends[newBackend];
    this.backendChange();
  };

  inputChange = () => {
    const newInput = this.getEditor(this.inputEditorId).getValue();
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

      for (const [lang, { syntax, hover, format, autoFormat }] of Object.entries(
        this.props.languageRegistrations
      )) {
        monaco.languages.register({ id: lang });
        monaco.languages.setMonarchTokensProvider(lang, syntax);
        if (hover) {
          monaco.languages.registerHoverProvider(lang, { provideHover: hover(monaco) });
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

      for (const editorId of Object.keys(this.editors)) {
        const extraOpts: monaco.editor.IStandaloneEditorConstructionOptions =
          this.editors[editorId].kind === "output"
            ? { readOnly: true }
            : { language: this.props.language };
        const editor = monaco.editor.create(document.getElementById(editorId)!, {
          ...editorOpts,
          theme: "pgtheme",
          ...extraOpts,
        });
        this.editors[editorId].editor = editor;

        if (this.editors[editorId].kind === "input") {
          editor.setValue(this.props.examples[this.props.defaultExample]);
          editor.onDidChangeModelContent(this.inputChange);
        }
      }

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
    const getBackendEditorAt = (index: number) => this.getEditor(backendEditors[index]);
    const getInputEditor = () => this.getEditor(this.inputEditorId);

    this.registerEditor(this.inputEditorId, "input");

    return (
      <MdWrapper>
        <Box display="grid" gridTemplateColumns="1fr 1fr" gridGap={0}>
          <InputColumn
            source={this.props.source}
            grammar={this.props.grammar}
            examples={this.props.examples}
            defaultExample={this.props.defaultExample}
            backendOptions={Object.keys(this.props.backends)}
            defaultBackend={this.props.defaultBackend}
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
