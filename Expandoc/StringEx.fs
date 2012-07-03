[<AutoOpen>]
module StringEx
type System.String with
    ///Performs case-insensitive string comparison using InvariantCultureIgnoreCase. 
    member s1.icompare(s2: string) =
        System.String.Equals(s1, s2, System.StringComparison.InvariantCultureIgnoreCase)

