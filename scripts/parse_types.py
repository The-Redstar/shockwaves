
import re


# re that matches identifier paths, including weird operators, but not if they are in strings
ID=re.compile(r"(?P<id>(?P<source>(?:[A-Z][A-Za-z\d_]*\.)*)(?P<obj>(?:\([\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]+\))|(?:[A-Z][A-Za-z\d_]*)))")
"""                          any minimal number of times
(?P<id>
    (?P<source>[A-Z][A-Za-z\d_]*\.)*    a number of CapitalisedIdentifier.*
    (?P<obj>                        followed by either
        (?:\([\!\#\$\%\&\*\+
           \.\/\<\=\>\?\@\\
           \^\|\-\~\:]+\)
        )                      an operator (...)
    |                          or
        (?:[A-Z][A-Za-z\d_]*)   CapitalisedIdentifier
    )
)
"""

RE_STR=r'"(?:(?:(?!(?<!\\)").)*)"'
RE_CHR=r"'(?:(?:(?!(?<!\\)').)*)'"

def parse_type(ty:str) -> tuple[str,list[str]]:
    """
    Take a type description string, and return the Haskell code to create it, as well as a list of required imports
    """

    # replace tuple constructors, because they don't want to work????
    for i in range(1,16):
        ty=ty.replace("GHC.Tuple.({})".format(","*i),"tupleConstr")

    # find identifier sources
    ty=re.sub(RE_STR,"",ty) #remove strings # TODO: WILL FAIL IF THE " IS IN A CHAR
    ty=re.sub(RE_CHR,"",ty) #remove chars
    sources = set(m[1][:-1] for m in re.findall(ID,ty) if m[1])

    return ty, sources

if __name__=="__main__":
    print(parse_type("((Clash.Signal.Internal.Signal \"System\") ((Clash.Sized.Vector.Vec 4) (Clash.Sized.Internal.Signed.Signed 32)))"))
    print()
    
    print(parse_type("((GHC.Tuple.(,) ((Clash.Sized.Vector.Vec 4) (Clash.Sized.Internal.Signed.Signed 32))) ((Clash.Sized.Vector.Vec 2) (Clash.Sized.Internal.Signed.Signed 32)))"))
    