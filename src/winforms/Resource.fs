module Resource

open System.Drawing
open System.Reflection
open System.Resources

let executingAssembly = Assembly.GetExecutingAssembly()

let resources = new ResourceManager("Winforms.res", executingAssembly)

let stringResource = resources.GetString

let title = stringResource "title"

let about = stringResource "about"

let icon = resources.GetObject("icon") :?> Icon

let version = stringResource "version"