module Parser

open Cradle

type ScanState = { 
    mutable look: char
    reader: unit -> char
    mutable output: string
}

type ScanState with
    static member init r = {
        reader = r
        look = r() 
        output = ""
    }

    member this.peek discriminator expectedErr =
        if not (discriminator this.look) then Left(expected expectedErr)
        else Right(this)

    member this.writer s = this.output <- this.output + "\t" +  s + "\n"

    member this.peekNext(next: char) = 
        this.peek
            (fun s -> s = next)
            (next.ToString())

    member this.get discriminator err = 
        if not (discriminator this.look) then Left(expected err)
        else 
            let name = this.look
            this.look <- this.reader()
            Right(name, this)

    member this.matchNext(next: char) = 
        let result = 
            this.get
                (fun s -> s = next)
                (next.ToString())

        result.map(fun (_, s) -> s)

    member this.getName () = 
        let rec helper acc =
            if not (isAlNum this.look) then
                acc
            else 
                let s = this.look
                this.look <- this.reader()
                helper (acc + s.ToString())

        if not (isAlpha this.look) then
            Left (expected "Name")
        else Right (helper "", this)

    member this.getNum () = 
        let rec helper acc =
            if not (isDigit this.look) then
                acc
            else
                let s = this.look
                this.look <- this.reader()
                helper (acc + s.ToString())

        if not (isDigit this.look) then 
            Left (expected "Integer")
        else Right (helper "", this)

    member this.addToOutput s =
        this.output <- this.output + s



