module Fargo.Console

open System
open System.Runtime.InteropServices

module Native =
    let STD_OUTPUT_HANDLE = -11
    let ENABLE_VIRTUAL_TERMINAL_PROCESSING = 4u
    let DISABLE_NEWLINE_AUTO_RETURN = 8u
 
    [<DllImport("kernel32.dll", SetLastError=true)>]
    extern bool GetConsoleMode(IntPtr hConsoleHandle , uint32& lpMode) 
    [<DllImport("kernel32.dll", SetLastError=true)>]
    extern IntPtr GetStdHandle(int nStdHandle)
    [<DllImport("kernel32.dll", SetLastError=true)>]
    extern bool SetConsoleMode(IntPtr hConsoleHandle, uint32 dwMode )


let checkWin32VT100 () =
    let h =  Native.GetStdHandle(Native.STD_OUTPUT_HANDLE)
    let mutable x = 0u
    let r = Native.GetConsoleMode(h, &x)
    if not r then
        false
    elif x &&& Native.ENABLE_VIRTUAL_TERMINAL_PROCESSING <> 0u then
        true
    else
        let r = Native.SetConsoleMode(h, x ||| Native.ENABLE_VIRTUAL_TERMINAL_PROCESSING ||| Native.DISABLE_NEWLINE_AUTO_RETURN)
        r 

let supportVT100 =
    RuntimeInformation.IsOSPlatform OSPlatform.Linux ||
    RuntimeInformation.IsOSPlatform OSPlatform.OSX ||
    checkWin32VT100 ()

module Colors =
    let esc = "\x1B"
    let color n = if supportVT100 then $"{esc}[%d{n}m" else ""
    let def = color 0
    let red = color 31
    let yellow = color 33

