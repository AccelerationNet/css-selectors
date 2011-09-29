%option case-insensitive

h	      =>   [0-9a-f]
nmstart	      =>   [a-z]
nmchar	      =>   [a-z0-9-]
nl	      =>   \n|\r\n|\r|\f
string1	      =>   \"([\t !#$%&(-~]|\\{nl}|\')*\"
string2	      =>   \'([\t !#$%&(-~]|\\{nl}|\")*\'
id	      =>   [-]?{nmstart}{nmchar}*
name	      =>   {nmchar}+
int	      =>   [0-9]+
num	      =>   [0-9]+|[0-9]*\.[0-9]+
string	      =>   {string1}|{string2}
url	      =>   ([!#$%&*-~])*
w	      =>   [ \t\r\n\f]*
s             =>   [\s\f]+

%%

comment       =>   \/\*[^*]*\*+([^/][^*]*\*+)*\/
cdo           =>   <!--
cdc           =>   -->
class         =>   \.(?<ident>)
hash          =>   #(?<ident>{name})
ident         =>   {id}
element       =>   {id}
important_sym =>   !{w}important

selector        =>   (?<or>)
or              =>   (?<comb-sel>)(\s*,\s*(?<or>))?

comb-sel        =>   (?<and>)((?<combinator>)(?<comb-sel>))?
combinator      =>   (?<child>\s+)|(?<immediate-child>>)|(?<preceded-by>~)|(?<immediatly-preceded-by>\+)
and             =>   (?<sim-sel>)(?<and>)?
sim-sel         =>   (?<hash>)|(?<class>)|(?<element>)|\*|(?<attrib>)|(?<pseudo>)
attrib          =>   \[\s*(?<ident>)\s*(?<attrib-val>)?\]
attrib-val      =>   [\^\$\*\|\~]?=((?<ident>)|(?<string>))
pseudo          =>   :(?<ident>)(?<parens>\s*(?<selector>)|(?<nth-expr>)\s*)?
nth-expr        =>   even|odd|[\+-]?{int}(n([\+-]{int})?)?
