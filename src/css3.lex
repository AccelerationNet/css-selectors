%option case-insensitive

h		[0-9a-f]
nmstart		[a-z]
nmchar		[a-z0-9-]
nl		\n|\r\n|\r|\f
string1		\"([\t !#$%&(-~]|\\{nl}|\')*\"
string2		\'([\t !#$%&(-~]|\\{nl}|\")*\'

ident		[-]?{nmstart}{nmchar}*
name		{nmchar}+
int		[0-9]+
num		[0-9]+|[0-9]*\.[0-9]+
string		{string1}|{string2}
url		([!#$%&*-~])*
w		[ \t\r\n\f]*
range		\?{1,6}|{h}(\?{0,5}|{h}(\?{0,4}|{h}(\?{0,3}|{h}(\?{0,2}|{h}(\??|{h})))))

%%

[\s\f]+		{return S;}

\/\*[^*]*\*+([^/][^*]*\*+)*\/	/* ignore comments */

"<!--"			{return CDO;}
"-->"			{return CDC;}
"~="			{return INCLUDES;}
"|="			{return DASHMATCH;}
"$="			{return ENDS_WITH;}
"^="			{return BEGINS_WITH;}
"*="			{return SUBSTRING;}

nth-(child|last-child|of-type|nth-last-of-type)"("			{return NTH-FUNCTION;}
{ident}"("		{return FUNCTION;}
{string}		{return STRING;}
{ident}			{return IDENT;}

"."{ident}		{return CLASS;}
"#"{name}		{return HASH;}

"@import"		{return IMPORT_SYM;}
"@page"			{return PAGE_SYM;}
"@media"		{return MEDIA_SYM;}
"@font-face"		{return FONT_FACE_SYM;}
"@charset"		{return CHARSET_SYM;}
"@namespace"		{return NAMESPACE_SYM;}

"!"{w}"important"		{return IMPORTANT_SYM;}

{int}							{return INTEGER;}
{num}em			{return EMS;}
{num}ex			{return EXS;}
{num}px			{return LENGTH;}
{num}cm			{return LENGTH;}
{num}mm			{return LENGTH;}
{num}in			{return LENGTH;}
{num}pt			{return LENGTH;}
{num}pc			{return LENGTH;}
{num}deg		{return ANGLE;}
{num}rad		{return ANGLE;}
{num}grad		{return ANGLE;}
{num}ms			{return TIME;}
{num}s			{return TIME;}
{num}Hz			{return FREQ;}
{num}kHz		{return FREQ;}
{num}{ident}		{return DIMEN;}
{num}%			{return PERCENTAGE;}
{num}			{return NUMBER;}

"url("{w}{string}{w}")"	{return URI;}
"url("{w}{url}{w}")"	{return URI;}

U\+{range}		{return UNICODERANGE;}
U\+{h}{1,6}-{h}{1,6}	{return UNICODERANGE;}

.			{return *yytext;}
