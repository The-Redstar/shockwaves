
import re


# re that matches identifier paths, including weird operators, but not if they are in strings
ID=re.compile(r"(?P<id>(?P<source>(?:[A-Z][A-Za-z\d_]*\.)*)(?P<obj>(?:\([\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:,]+\))|(?:[A-Z][A-Za-z\d_]*)))")
"""                          any minimal number of times
(?P<id>
    (?P<source>[A-Z][A-Za-z\d_]*\.)*    a number of CapitalisedIdentifier.
    (?P<obj>                        followed by either
        (?:\([\!\#\$\%\&\*\+
           \.\/\<\=\>\?\@\\
           \^\|\-\~\:,]+\)
        )                      an operator (...)
    |                          or
        (?:[A-Z][A-Za-z\d_]*)   CapitalisedIdentifier (not followed by a period: add `(?![\.A-Za-z\d_])`)
    )
)
"""

RE_STR=r'"(?:(?:(?!(?<!\\)").)*)"'
RE_CHR=r"'(?:(?:(?!(?<!\\)').)*)'"

def parse_type(ty:str) -> tuple[str,list[str]]:
    """
    Take a type description string, and return the Haskell code to create it, as well as a list of required imports
    """

    # replace tuple constructors, because the library is hidden
    for i in range(1,64):
        ty=ty.replace("GHC.Tuple.Prim.({})".format(","*i),"(%s)"%(","*i))

    # find identifier sources
    ty2=re.sub(RE_STR,"",ty) #remove strings # TODO: WILL FAIL IF THE " IS IN A CHAR
    ty2=re.sub(RE_CHR,"",ty2) #remove chars
    sources = set(m[1][:-1] for m in re.findall(ID,ty2) if m[1])

    return ty, sources

if __name__=="__main__":
    print(parse_type("((Clash.Signal.Internal.Signal \"System\") ((Clash.Sized.Vector.Vec 4) (Clash.Sized.Internal.Signed.Signed 32)))"))
    print()
    
    ty="((GHC.Tuple.Prim.(,) ((Clash.Sized.Vector.Vec 4) (Clash.Sized.Internal.Signed.Signed 32))) ((Clash.Sized.Vector.Vec 2) (Clash.Sized.Internal.Signed.Signed 32)))"
    print(parse_type(ty))
    #print(re.findall(ID,ty))
    #print(re.findall(r"(?:\([\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:,]+\))",ty))
    #print(re.findall(r"[A-Z][A-Za-z\d_]*(?![\.A-Za-z\d_])",ty))