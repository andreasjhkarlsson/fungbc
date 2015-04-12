module Resource

open System.Drawing
open System.Reflection
open System.Resources
open System.Windows.Forms


let executingAssembly = Assembly.GetExecutingAssembly()
let resources = new ResourceManager("Resources", executingAssembly)

let stringResource = resources.GetString

let title = stringResource "title"

let about = stringResource "about"

let icon = resources.GetObject("icon") :?> Icon
