%{
#define LIST     strcat(buf,yytext)
#define token(t) {LIST;return(t);}
#define tokenReal(t,r) {LIST;return(t);} 
#define tokenInteger(t,i) {LIST;return(t);}
#define tokenString(t,s) {LIST;return(t);} 

//建立一個id的變數型態
//一個id包含了name，type，scope，是否為Dimension，是否為Pointer，是否有Parameter
struct idtype{
    char name[20];
    char type[20];
    char scope[20];
    char attrDimen[20];
    char attrPoint[20];
    char attrParam[20];
};

#define MAX_LINE_LENG 256
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "y.tab.h"

int linenum = 1;
char buf[MAX_LINE_LENG]; 
char temp[20];

int yywrap(void);

%}

letter [A-Za-z]
digit [0-9]
id ({letter}({letter}|{digit})*)

%x cmt singlecmt

%%
^"C".*  {BEGIN singlecmt;LIST;}
^"*".*   {BEGIN singlecmt;LIST;}
"!".*   {BEGIN singlecmt;LIST;}
<singlecmt>.    {LIST;}
<singlecmt>[\n]   {LIST;
                printf("Line %d Done !\n", linenum++);
                buf[0] = '\0';
                BEGIN 0;}

"/*" {BEGIN cmt;LIST;}
<cmt>[^*\n]*     {LIST;}
<cmt>"*"+[^*/\n]* {LIST;}
<cmt>"*"+"/"   {BEGIN 0;LIST;}
<cmt>\n         {LIST;
                printf("Line %d Done !\n", linenum++);
                buf[0] = '\0';}

"int"       {return(INT);}
"call"      {return(CALL);}
"case"      {return(CASE);}
"character" {return(CHARACTER);}
"close"     {return(CLOSE);}
"continue"  {return(CONTINUE);}
"cycle"     {return(CYCLE);}
"data"      {return(DATA);}
"dimension" {return(DIMENSION);}
"do"        {return(DO);}
"else"      {return(ELSE);}
"end"       {return(END);}
"exit"      {return(EXIT);}
"function"  {return(FUNCTION);}
"goto"      {return(GOTO);}
"if"        {return(IF);}
"integer"   {return(INTEGER);}
"interface" {return(INTERFACE);}
"logical"   {return(LOGICAL);}
"procedure" {return(PROCEDURE);}
"parameter" {return(PARAMETER);}
"print"     {return(PRINT);}
"println"   {return(PRINTLN);}
"pointer"   {return(POINTER);}
"program"   {return(PROGRAM);}
"real"      {return(REAL);}
"recursive" {return(RECURSIVE);}
"return"    {return(RETURN);}
"select"    {return(SELECT);}
"stop"      {return(STOP);}
"string"    {return(STRING);}
"subroutine"    {return(SUBROUTINE);}
"type"      {return(TYPE);}
"then"      {return(THEN);}

"."     {return(*yytext);} 
"^"     {return(*yytext);}
"?"     {return(*yytext);}
"+"     {return(*yytext);}
"-"     {return(*yytext);}
"*"     {return(*yytext);}
"/"     {return(*yytext);}
"="     {return(*yytext);}
"|"     {return(*yytext);}
"("     {return(*yytext);}
")"     {return(*yytext);}
"["     {return(*yytext);}
"]"     {return(*yytext);}
"{"     {return(*yytext);}
"}"     {return(*yytext);}
"%"     {return(*yytext);}
","     {return(*yytext);}
":"     {return(*yytext);}
";"     {return(*yytext);}

"=>"    {return(EQB);}
"**"    {return(DS);}
"::"    {return(EXP);}
("<"|".LT.")    {return(LT);}
(">"|".GT.")    {return(GT);}
("<="|".LE.")   {return(LE);}
(">="|".GE.")   {return(GE);}
("=="|".EQ.")   {return(EQ);}
("/="|".NE.")   {return(NE);}
("||"|".OR.")   {return(OR);}
("&&"|".AND.")  {return(AND);}
".NOT."         {return(NOT);}
".EQV."         {return(EQV);}
".NEQV."        {return(NEQV);}
"TRUE"          {return(TRUE);}
"FALSE"         {return(FALSE);}

{digit}+ {
        yylval.ival=atoi(yytext);
        return(IntegerNum);
} //讀到integer時，值存到yylval.ival裡，回傳IntegerNum


("+"|"-")?{digit}+"."{digit}*("e")?("+"|"-")?{digit}+ {
        yylval.fval=atof(yytext);
        return(FloatNum);
} //讀到real時，值存到yylval.fval裡，回傳FloatNum


{id}	{												
        int i;
        for(i=0;i<yyleng;i++){
                temp[i]=tolower(yytext[i]);				
        }
        temp[yyleng]='\0';
        strcpy(yylval.idval.name,temp);
        return(ID);						
} //讀到ID時，先將所有字轉成小寫，再存到yylval.idval.name裡，回傳ID


\"[^\n]*[\\"]*\" {
    strcpy(yylval.sval,yytext);
    return(STRING);
    } //讀到STRING時，直接回傳STRING(目前只有print會用到)


[ \t]* {LIST;} //讀到空白跟分段符號時，不理會


\n   {
        LIST;
        printf("Line %d Done !\n", linenum++);
        buf[0] = '\0';
        }	 //讀到換行符號時，輸出行數，以供辨認									

.       {
        LIST;
        printf("%d:%s\n", linenum+1, buf);
        printf("bad character:'%s'\n",yytext);
        exit(-1);
        }
%%

int yywrap(void){
        printf("Line %d Done !\n", linenum++);
        return 1;
}