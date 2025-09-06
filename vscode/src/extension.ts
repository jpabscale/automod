import * as vscode from "vscode";

const defaultMaxLogs = 30
const isWindows = process.platform === "win32";
const fsep = isWindows ? "\\" : "/";
const configFilename = ".config.json"
const tempDir = isWindows? `${process.env.LOCALAPPDATA}${fsep}Temp${fsep}automod` : `${process.env.HOME}/.local/share/Temp/automod`

interface Tools {
  retoc: string
  repak: string
  uassetCli: string
  fmodel: string
  jd: string
}

interface Game {
  directory: string
  contentPaks: string
  unrealEngine: string
  mapUri: string
  aesKey: string
  zen: boolean
  repakPackOptions: string
}

interface Games {
  [key: string]: Game
}

interface Config {
  games: Games
  tools: Tools
}

interface TaskData {
  type: string,
  kind: string,
  args: string[]
}

function gameEquals(g1: Game, g2: Game): boolean {
  return g1.contentPaks == g2.contentPaks &&
    g1.unrealEngine == g2.unrealEngine &&
    g1.mapUri == g2.mapUri &&
    g1.aesKey == g2.aesKey &&
    g1.zen == g2.zen &&
    g1.repakPackOptions == g2.repakPackOptions;
}

function toolEquals(t1: Tools, t2: Tools): boolean {
  return t1.retoc == t2.retoc && 
    t1.repak == t2.repak &&
    t1.fmodel == t2.fmodel &&
    t1.jd == t2.jd &&
    t1.uassetCli == t2.uassetCli;
}

function configEquals(c1: Config, c2: Config): boolean {
  if (!toolEquals(c1.tools, c2.tools)) return false;
  const keys1 = Object.keys(c1.games).toSorted();
  const keys2 = Object.keys(c2.games).toSorted();
  if (JSON.stringify(keys1) != JSON.stringify(keys2)) return false;
  for (const key of keys1) if (!gameEquals(c1.games[key], c2.games[key])) return false;
  return true;
}

interface SettingAutomod {
  "Installation Directory": string
  "No Parallelization": boolean
  "Max Logs": number
  "Scala CLI Server": boolean
  "Active Game ID": string
}

interface SettingAutomodModGen {
  "Mod Name": string
  "No Code Patching": boolean
  "Dry Run": boolean
  "Include Patches": boolean
  "Ultra Compression": boolean
}

interface SettingAutomodGames {
  [key: string]: string
}

async function exists(path: string): Promise<boolean> {
  try {
    await vscode.workspace.fs.stat(vscode.Uri.file(path));
    return true;
  } catch {    
    return false;
  }
}

async function read(path: string): Promise<string> {
  return new TextDecoder('utf-8').decode(await vscode.workspace.fs.readFile(vscode.Uri.file(path)));
}

function getWorkspacePath(): string | undefined {
  return vscode.workspace.workspaceFolders?.[0].uri.fsPath;
}

async function getConfigPath(workspaceOnly: Boolean): Promise<string | undefined> {
  let r = `${getWorkspacePath()}${fsep}${configFilename}`;
  if (workspaceOnly) return r;
  if (await exists(r)) return r;
  r = `${await getAutomodDir()}${fsep}${configFilename}`
  return await exists(r)? r : undefined; 
}

async function setting<T>(group: string, id: string, value: T | undefined, target: vscode.ConfigurationTarget = vscode.ConfigurationTarget.Global): Promise<T | undefined> {
  const r = vscode.workspace.getConfiguration(group).get<T>(id); 
  if (value) {
    if (target == vscode.ConfigurationTarget.Global || target == vscode.ConfigurationTarget.Workspace && await getWorkspacePath())
      await vscode.workspace.getConfiguration(group).update(id, value, target);
  }
  return r;
}

async function settingAutomodDir(value: string | undefined = undefined): Promise<string | undefined> {
  return await setting<string>("automod", "installationDirectory", value);
}

async function settingAutomod(value: SettingAutomod | undefined = undefined): Promise<SettingAutomod> {
  const r = await setting<SettingAutomod>("automod", "general", value, vscode.ConfigurationTarget.Workspace);
  return r!;
}

async function settingAutomodModGeneration(value: SettingAutomodModGen | undefined = undefined): Promise<SettingAutomodModGen> {
  const r = await setting<SettingAutomodModGen>("automod", "modGeneration", value);
  return r!;
}

async function settingAutomodTools(value: Tools | undefined = undefined): Promise<Tools> {
  const r = await setting<Tools>("automod", "tools", value);
  return r!;
}

async function settingAutomodGames(merge: boolean, value: Games | undefined = undefined): Promise<Games> {
  let games: SettingAutomodGames | undefined = undefined;
  if (value) {
    const keys = Object.keys(value).toSorted();
    games = {};
    if (merge) {
      const old = await setting<SettingAutomodGames>("automod", "games", undefined);
      if (old) games = old;
    }
    for (const key of keys) {
      const game = value[key];
      games[`${key}.directory`] = game.directory;
      games[`${key}.contentPaks`] = game.contentPaks;
      games[`${key}.unrealEngine`] = game.unrealEngine;
      games[`${key}.mapUri`] = game.mapUri;
      games[`${key}.aesKey`] = game.aesKey;
      games[`${key}.zen`] = game.zen.toString();
      games[`${key}.repakPackOptions`] = game.repakPackOptions;
    }
  }
  const newGames = (await setting<SettingAutomodGames>("automod", "games", games))!;
  let r: Games = {};
  const keys = Object.keys(newGames).toSorted();
  for (const key of keys) {
    const array = key.split(".", 2); 
    const gameId = array[0].trim();
    const property = array[1].trim();
    const game: Game = r[gameId]? r[gameId] : { directory: "", contentPaks: "", unrealEngine: "", mapUri: "", aesKey: "", zen: true, repakPackOptions: "" }; 
    switch(property) {
      case "directory":
        game.directory = newGames[key];
        break;
      case "contentPaks":
        game.contentPaks = newGames[key];
        break;
      case "unrealEngine":
        game.unrealEngine = newGames[key];
        break;
      case "mapUri":
        game.mapUri = newGames[key];
        break;
      case "aesKey":
        game.aesKey = newGames[key];
        break;
      case "zen":
        switch(newGames[key]) {
          case "true":
            game.zen = true;
            break;
          case "false":
            game.zen = false;
            break;
          default:
            vscode.window.showInformationMessage(`Invalid value for ${key}: ${newGames[key]}`);
        }
        break;
      case "repakPackOptions":
        game.repakPackOptions = newGames[key];
        break;
      default:
        vscode.window.showInformationMessage(`Invalid property ${key}`);
    }
    r[gameId] = game;
  }
  return r;
}

async function readFile(configPath: string): Promise<string | undefined> {
  try {
    return new TextDecoder('utf-8').decode(await vscode.workspace.fs.readFile(vscode.Uri.file(configPath)));
  } catch {
    return undefined;
  }
}

async function getConfigFileContent(workspaceOnly: boolean): Promise<string | undefined> {
  const configPath = await getConfigPath(workspaceOnly);
  if (!configPath) {
    return undefined;
  }
  return await readFile(configPath);
}

async function loadConfigH(merge: boolean, configPath: string): Promise<boolean> {
  try {
    const content = await readFile(configPath);
    if (!content) return false;
    const config = JSON.parse(content) as Config;
    await settingAutomodGames(merge, config.games);
    await settingAutomodTools(config.tools);
    if (merge) vscode.window.showInformationMessage(`Successfully imported ${configPath}`)
    return true;
  } catch {
    vscode.window.showInformationMessage(`Could not import ${configPath}`)
    return false
  }
}

async function loadConfig(): Promise<boolean> {
  const configPath = await getConfigPath(false)
  if (!configPath) return false;
  const r = await loadConfigH(false, configPath);
  if (!r) vscode.window.showInformationMessage(`Could not load automod .config.json`);
  return r
}

async function getConfig(): Promise<Config | undefined> {
  const r: Config = {
    games: await settingAutomodGames(false),
    tools: await settingAutomodTools()
  };
  return r;
}

async function getConfigString(): Promise<string | undefined> {
  const config = await getConfig();
  return config? JSON.stringify(config, null, "  ") : undefined;
}

async function writeFile(path: string, content: string) {
  await vscode.workspace.fs.writeFile(vscode.Uri.file(path), new TextEncoder().encode(content));
  await vscode.commands.executeCommand("workbench.files.action.refreshFilesExplorer");
}

async function updateConfig() {
  if (!getWorkspacePath()) return;
  const configInFile = (await getConfigFileContent(true))!;
  const currentConfig = (await getConfig())!;
  const configPath = (await getConfigPath(true))!;
  if (configInFile) {
    if (!configEquals(JSON.parse(configInFile) as Config, currentConfig)) {
      if (await exists(configPath)) {
        try {
           const backup = `${configPath.substring(0, configPath.length - ".json".length)}-${getTimestamp()}.json`;
           await vscode.workspace.fs.rename(vscode.Uri.file(configPath), vscode.Uri.file(backup), {overwrite: true});
           vscode.window.showInformationMessage(`Wrote updated .config.json and the previous .config.json was backed up`);
        } catch {
          vscode.window.showInformationMessage(`Failed to make a backup of .config.json that differs from the settings`);
          return; 
        }
      }
      await writeFile(configPath, JSON.stringify(currentConfig, null, "  "));
    }
  } else {
    await writeFile(configPath, JSON.stringify(currentConfig, null, "  "));
    vscode.window.showInformationMessage(`Wrote updated .config.json`);
  }
}

function newTask(data: TaskData): vscode.Task {
  const wf = getWorkspacePath();
  const t = new vscode.Task({ type: data.type, kind: data.kind }, vscode.TaskScope.Workspace, data.kind, data.type,
    new vscode.ShellExecution(data.args[0], data.args.slice(1), isWindows? { 
      executable: "cmd.exe", 
      shellArgs: ["/D", "/C"], 
      cwd: wf
    }: { 
      cwd: wf
    }), []);
  t.presentationOptions = {
    echo: true,
    focus: true,
    panel: vscode.TaskPanelKind.Dedicated,
    clear: false,
    showReuseMessage: false,
    reveal: vscode.TaskRevealKind.Always
  };
  return t;
}

function getTimestamp(): string {
  return (new Date()).toISOString().replaceAll(":", "-").replaceAll(".", "-");
}

function getOutputDir(): string {
  return "${workspaceFolder}" + `${fsep}vscode-out`;
}

async function getCommandPrefix(): Promise<string[]> {
  const automodDir = await getAutomodDir();
  const r: string[] = [ isWindows? `${automodDir}\\automod.cmd` : `${automodDir}/automod` ];
  const setting = await settingAutomod();
  if (!setting["Scala CLI Server"]) r.push("-s");
  if (setting["Active Game ID"].length > 0) {
    r.push("-g");
    r.push(setting["Active Game ID"]);
  }
  if (setting["No Parallelization"]) {
    r.push("-p");
  }
  r.push("-l");
  r.push(setting["Max Logs"].toString());
  return r;
}

async function hasPatchesDir(): Promise<boolean> {
  return await exists(`${getWorkspacePath()}${fsep}patches`);
}

export class AutomodTaskProvider implements vscode.TaskProvider {
  static TYPE = "automod";

  async getTasks(): Promise<vscode.Task[]> {
    const automodDir = await getAutomodDir();
    if (!automodDir) return [];
    const type = AutomodTaskProvider.TYPE;
    const output = getOutputDir();
    const settingModGen = await settingAutomodModGeneration();
    const games = await settingAutomodGames(false);
    const setting = await settingAutomod();
    const cmdPrefix = await getCommandPrefix();
    const tasks: vscode.Task[] = [];
    const isNotUndefined = <T>(value: T | undefined): value is T => value !== undefined;
    const hasPatches = await hasPatchesDir();
    let gameId = "SB";
    if (setting["Active Game ID"].length > 0) gameId = setting["Active Game ID"];
    if (!games[gameId]) vscode.window.showInformationMessage(`Could not find configuration for the game identifier: ${gameId}`);
    if (hasPatches) {
      const options = [];
      if (settingModGen["No Code Patching"]) options.push("--no-code-patching");
      if (settingModGen["Dry Run"]) options.push("--dry-run");
      if (settingModGen["Include Patches"]) options.push("--include-patches")
      if (settingModGen["Ultra Compression"]) options.push("--ultra-compression")
      tasks.push(newTask({type: type, kind: ".batch", args: [ ...cmdPrefix, ".batch"].concat(options)}));
      if (settingModGen["Mod Name"].length > 0) tasks.push(newTask({type: type, kind: settingModGen["Mod Name"], args: [ ...cmdPrefix, settingModGen["Mod Name"]].concat(options)}));
    }
    tasks.push(newTask({type: type, kind: ".demo.sb", args: [ ...cmdPrefix, ".demo.sb"]}));
    tasks.push(newTask({type: type, kind: ".demo.soa", args: [ ...cmdPrefix, ".demo.soa"]}));
    if (vscode.window.activeTextEditor?.document.fileName.endsWith(".sam"))
      tasks.push(newTask({type: type, kind: ".search", args: [ ...cmdPrefix, ".search", "${file}", `${output}${fsep}search-\${fileBasenameNoExtension}-${getTimestamp()}`].filter(isNotUndefined)}));
    if (hasPatches) tasks.push(newTask({type: type, kind: ".toml", args: [ ...cmdPrefix, ".toml", `${output}${fsep}toml-${getTimestamp()}`].filter(isNotUndefined)}));
    if (hasPatches) tasks.push(newTask({type: type, kind: ".toml.all", args: [ ...cmdPrefix, ".toml.all", `${output}${fsep}toml.all-${getTimestamp()}`].filter(isNotUndefined)}));
    tasks.push(newTask({type: type, kind: ".upgrade", args: [ ...cmdPrefix, ".upgrade"]}));
    return tasks;
  }

  async provideTasks(): Promise<vscode.Task[]> {
    if (await getConfigString()) return this.getTasks();
    return [];
  }

  resolveTask(_task: vscode.Task): vscode.Task | undefined {
    return undefined;
  }
}


async function getAutomodDir(): Promise<string | undefined> {
  let dir = await settingAutomodDir();
  if (dir && dir.length == 0) {
    const p = getWorkspacePath(); 
    dir = p? p: "";
  } 
  let automodScript = `${dir}${fsep}automod.sc`;
  if (await exists(automodScript)) {
    await settingAutomodDir(dir);
    return dir;
  }
  const temp = `${tempDir}${fsep}.automod.dir`
  if (await exists(temp)) {
    dir = await read(temp);
    automodScript = `${dir}${fsep}automod.sc`;
    if (await exists(automodScript)) {
      await settingAutomodDir(dir);
      return dir;
    }
  }

  return undefined;
}

export async function activate(_context: vscode.ExtensionContext) {
  if (!await getAutomodDir()) {
    vscode.window.showInformationMessage(`Please configure the automod installation directory`)
    return;
  }
  if (!await loadConfig()) return;
  const modName = (await settingAutomodModGeneration())["Mod Name"];
  if (modName.length == 0 && await hasPatchesDir()) 
    vscode.window.showInformationMessage(`Please configure your mod name in the automod Preferences/Settings`);
  
  vscode.tasks.registerTaskProvider(AutomodTaskProvider.TYPE, new AutomodTaskProvider());

  for (const id of ["diff", "diff.into"]) {
    vscode.commands.registerCommand(`automod.${id}`, async (_contextSelection: vscode.Uri, allSelections: vscode.Uri[]) => {    
      if (!await getAutomodDir()) {
        vscode.window.showInformationMessage(`Please configure the automod installation directory`);
        return;
      }
      if (allSelections.length == 2) {
        if (!await getConfigString()) {
          vscode.window.showInformationMessage(`Could not find automod .config.json in the workspace root`);
          return;
        }
        const first = allSelections[0].fsPath;
        const second = allSelections[1].fsPath;
        let output = getOutputDir();
        output = `${output}${fsep}${id}-${getTimestamp()}`;
        if (id == "diff") {
          output = `${output}${fsep}${second.substring(second.lastIndexOf(fsep) + 1)}`
        }
        const cmdPrefix = await getCommandPrefix();
        const taskData: TaskData = {type: AutomodTaskProvider.TYPE, kind: `.${id}`, args: [...cmdPrefix, `.${id}`, first, second, output]};
        vscode.tasks.executeTask(newTask(taskData));
      } else {
        vscode.window.showInformationMessage(`Please select two directories for automod .${id}`);
      }
    });
  }
  vscode.commands.registerCommand("automod.config.import", async (_contextSelection: vscode.Uri, allSelections: vscode.Uri[]) => {    
    const options: vscode.OpenDialogOptions = { 
      canSelectMany: false, canSelectFolders: false, openLabel: 'Select automod .config.json',
      filters: {
        'JSON files': ['json']
      }
    };
    vscode.window.showOpenDialog(options).then(fileUri => { if (fileUri && fileUri[0]) loadConfigH(true, fileUri[0].fsPath) });   
  });
  vscode.workspace.onDidChangeConfiguration(async event => {
    let affected = event.affectsConfiguration("automod.games") || event.affectsConfiguration("automod.tools");
    if (affected) await updateConfig();
  });
}