module Units

// Hertz
[<Measure>] type Hz

[<Measure>] type byte

let kB = 1024<byte>

let MB = 1024 * kB

let GB = 1024 * MB

let TB = 1024 * GB // Like this is ever gonna be used :D