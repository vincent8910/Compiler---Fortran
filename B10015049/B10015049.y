%{
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "lex.yy.c"
#define Trace(t) if (Opt_P) printf(t)

FILE *f;

int Opt_P = 1;
int SicDecType=0;   //用來判斷宣告一個變數時，該型態為何(INTEGER=1,REAL=2,LOGICAL=3,CHARACTER=4)
int SicDimen=0;     //用來判斷宣告一個變數時，該變數是否為Dimension
int SicPoint=0;     //用來判斷宣告一個變數時，該變數是否為Pointer
int SicParam=0;     //用來判斷宣告一個變數時，該變數是否為Parameter
int TableTop=0;     //標記目前Symbol Table的高度
struct idtype Table[256];   //一個以自訂義結構"idtype"為主體的SymbolTable

int yyerror(char*);
void CDecType(char[20]);                        //判斷型態的副程式
void CDecScope(char[20],char[20],char[20]);     //判斷屬性的副程式
void create();          //建立SymbolTable的副程式
int lookup(char[20]);   //在SymbolTable中搜尋ID的副程式
void insert(char[20]);  //在SymbolTable中建立新ID的副程式
void dump();            //印出整個SymbolTable的副程式
%}

%union {
     int ival;              //接受INTEGER型態的變數
     float fval;            //接受REAL型態的變數
     char cval;             //接受CHARACTER型態的變數
     struct idtype idval;   //接受ID自定義型態的變數
     char sval[50];         //接受STRING型態的變數
}

//以%token為開頭，定義常用關鍵字
%token INT CALL CASE CHARACTER CLOSE CONTINUE COMMON
%token CYCLE DATA DIMENSION DO ELSE END EXIT 
%token FUNCTION GOTO IF INTEGER INTERFACE LOGICAL PROCEDURE
%token PARAMETER PRINT PRINTLN POINTER PROGRAM
%token REAL RECURSIVE RETURN SELECT STOP
%token SUBROUTINE TYPE EXP THEN
%token TRUE FALSE EQB DS

//以%token為開頭，中間用<型態變數名稱>，後面加上回傳時要傳回的關鍵字
//例如lex回傳遇到10，回傳IntegerNum，得知是ival變數，從union得知是int型態
%token <ival> IntegerNum 
%token <fval> FloatNum
%token <idval> ID
%token <sval> STRING

//定義在expression中會用到的各種關鍵字的優先順序，其中UMINUS(負號)為最高
%left EQV NEQV
%left OR
%left AND
%left NOT
%left LT GT LE GE EQ NE '=' EQB
%left '+' '-'
%left '*' '/'
%left DS
%nonassoc UMINUS

%%

//程式以PROGRAM ID為開頭，中間依序宣告變數，宣告介面，加入敘述式，以END PROGRAM ID為結尾後，加入可能有的Procedures
program:    PROGRAM ID
            {
                if(lookup($2.name)==256){
                    insert($2.name);
                    strcpy(Table[TableTop-1].type,"PROGRAM");
                    CDecScope(Table[TableTop-1].attrDimen,Table[TableTop-1].attrPoint,Table[TableTop-1].attrParam);
                }
                fprintf(f,"class %s\n{\n",$2.name);
                fprintf(f,"\tmethod public static void main (java.lang.String[])\n");
                fprintf(f,"\tmax_stack 15\n");
                fprintf(f,"\tmax_locals 15\n\t{\n");
            }
            TopDataDec TopProcDec TopStmt END PROGRAM ID
            {
                fprintf(f,"\t\treturn\n\t}\n}");
            }
            TopProc  
            ;

//宣告Procedure時的結構，以INTEFACE為開頭，中間宣告Procedure，以END INTERFACE為結尾
TopProcDec: INTERFACE ProcDecs END INTERFACE
            | 
            ;

//有可能有多個Procedure 的宣告 在INTERFACE中
ProcDecs:   ProcDecs ProcDec
            | ProcDec
            ;

//Procedure的宣告包含Function的宣告 和 Subroutine的宣告
ProcDec:    FunctionDec
            | SubroutineDec
            ;

//建立Procedure
TopProc:    Procs
            |
            ;

//有可能有多個Procedure
Procs:  Procs Proc
        | Proc
        ;

//Procedure的宣告包含Function 和 Subroutine
Proc:   Function
        | Subroutine
        ;

//Function的宣告，以FUNCTION ID (arguments) 為開頭，宣告變數後，END FUNCTWION ID
FunctionDec:    FUNCTION ID '(' ids ')' TopDataDec END FUNCTION ID
                {if(lookup($2.name)==256) insert($2.name);}
                ;
//Function以FUNCTION ID (arguments) 為開頭，中間依序宣告變數，宣告介面，加入敘述式，END FUNCTION ID
Function: FUNCTION ID '(' ids ')' TopDataDec TopProcDec Stmts END FUNCTION ID
        ;

//Subroutine的宣告，以SUBROUTINE ID (arguments) 為開頭，宣告變數後，END SUBROUTINE ID
SubroutineDec:  SUBROUTINE ID '(' ids ')' TopDataDec END SUBROUTINE ID          
                {if(lookup($2.name)==256) insert($2.name);}
                ;

//Subroutine以SUBROUTINE ID (arguments) 為開頭，中間依序宣告變數，宣告介面，加入敘述式，END SUBROUTINE ID
Subroutine: SUBROUTINE ID '(' ids ')' TopDataDec TopProcDec Stmts END SUBROUTINE ID
            ;

//Procedure回傳值的格式
ProcBack: ID '(' ids ')'
        ;

//加入Statement
TopStmt:    Stmts
            |
            ;

//可能會有多個Statement
Stmts:  Stmts Stmt
        | Stmt
        ;

//Statement 的內容
Stmt:   Prints
        | ID '=' exp        
        {if(lookup($1.name)==256) yyerror("ID is not declare");}
        | RETURN
        | IfStmt
        | DoStmt
        ;

//If Statement 的結構
IfStmt: IF '(' booexp ')' THEN TopStmt ELSE TopStmt END IF
        | IF '(' booexp ')' THEN TopStmt END IF
        | IF '(' booexp ')' Stmt
        ;

//Do Statement 的結構
        DoStmt: DO ID '=' bootype ',' bootype Stmt END DO
        ;

//Print的各種使用方式
Prints: PRINT 
        {fprintf(f,"\t\tgetstatic java.io.PrintStream java.lang.System.out\n");}
        PrintsType
        {fprintf(f,"\t\tinvokevirtual void java.io.PrintStream.print(java.lang.String)\n");}
        | PRINTLN 
        {fprintf(f,"\t\tgetstatic java.io.PrintStream java.lang.System.out\n");}
        PrintsType
        {fprintf(f,"\t\tinvokevirtual void java.io.PrintStream.println(java.lang.String)\n");}
        | PRINT
        {fprintf(f,"\t\tgetstatic java.io.PrintStream java.lang.System.out\n");}
        '(' PrintsType ')'
        {fprintf(f,"\t\tinvokevirtual void java.io.PrintStream.print(java.lang.String)\n");}
        | PRINTLN
        {fprintf(f,"\t\tgetstatic java.io.PrintStream java.lang.System.out\n");}
        '(' PrintsType ')'
        {fprintf(f,"\t\tinvokevirtual void java.io.PrintStream.println(java.lang.String)\n");}
        ;

//可以被Print出來的各種型態
PrintsType: ID
            | IntegerNum
            | FloatNum
            | '-' ID %prec UMINUS
            | '-' IntegerNum %prec UMINUS
            | '-' FloatNum %prec UMINUS
            | STRING
            {
                fprintf(f,"\t\tldc %s\n",$1);
            }
            ;

//Expression運算式的結構
exp:    exp '+' exp
        | exp '-' exp
        | exp '*' exp
        | exp '/' exp
        | ID            {if(lookup($1.name)==256) yyerror("ID is not declare");}
        | IntegerNum
        | FloatNum
        | ProcBack
        ;

//宣告變數
TopDataDec: DataDecs
            |
            ;

//可能會有多個變數
DataDecs:   DataDecs DataDec
            | DataDec
            ;

//宣告變數的結構，如果有改變過屬性，要把判斷變數全部歸零
DataDec:    DataType DecTypeids                                                  
            | DataType Attr EXP DecTypeids  {SicDimen=0;SicPoint=0;SicParam=0;}                   
            | DATA '/' ID '/' FloatNum '/'
            | DATA '/' ID '/' IntegerNum '/'
            | COMMON '/' ID '/'
            ;

//用來宣告變數的四種主要變數型態，並改變判斷變數型態變數的值，在等等的ID中做改變
DataType:   INTEGER         {SicDecType=1;}
            | REAL          {SicDecType=2;}
            | LOGICAL       {SicDecType=3;}
            | CHARACTER     {SicDecType=4;}
            ;

//三種主要的屬性，若有特別宣告，則要在symbol中改變屬性
Attr:   ',' DIMENSION '(' IntegerNum ')'    {SicDimen=1;}
        | ',' POINTER                       {SicPoint=1;}
        | ',' PARAMETER                     {SicParam=1;}
        |
        ;

//判斷式的結構
booexp: bootype LT bootype
        | bootype GT bootype
        | bootype EQ bootype
        | bootype LE bootype
        | bootype GE bootype
        ;   

//可以用來判斷的變數型態
bootype:    ID
            {if(lookup($1.name)==256) yyerror("ID is not declare");}
            | IntegerNum
            | FloatNum
            | '-' ID %prec UMINUS
            | '-' IntegerNum %prec UMINUS
            | '-' FloatNum %prec UMINUS
            ;

//宣告變數時會用到的ID們，利用insert將ID加到SymbolTable中，並用CDecType來判斷變數型態，再用CDecScope來判斷變數屬性
DecTypeids:    DecTypeids ',' ID 
        {if(lookup($3.name)==256) {
            insert($3.name);
            CDecType(Table[TableTop-1].type);
            CDecScope(Table[TableTop-1].attrDimen,Table[TableTop-1].attrPoint,Table[TableTop-1].attrParam);
        }
        }
        | ID       
        {if(lookup($1.name)==256){
            insert($1.name);
            CDecType(Table[TableTop-1].type);
            CDecScope(Table[TableTop-1].attrDimen,Table[TableTop-1].attrPoint,Table[TableTop-1].attrParam);
            }   
        }
        ;

//一般會使用到的變數們
ids:    ids ',' ID
        | ID
        ;

%%

int main(){
    f = fopen("output.jasm", "w");

    create();
    yyparse();
    dump();
    fclose(f);
}

int yyerror(char* msg)
{
    printf("Error: %d,%s \n",linenum,msg);
    return 0;
}

//==================以下為型態、屬性檢查==================

//檢查SicDecType現在的數字，並依據數字不同給予不同的型態值
void CDecType(char s[20]){
    if(SicDecType == 1) strcpy(s,"INTEGER");
    else if(SicDecType == 2) strcpy(s,"REAL");
    else if(SicDecType == 3) strcpy(s,"LOGICAL");
    else if(SicDecType == 4) strcpy(s,"CHAR");
}

//檢查SicDimen、SicPoint、SicParam現在的數字，是0給NO,是1給YES
void CDecScope(char dimen[20],char point[20],char param[20]){
    if(SicDimen==1) strcpy(dimen,"Yes"); else strcpy (dimen,"No");
    if(SicPoint==1) strcpy(point,"Yes"); else strcpy (point,"No");
    if(SicParam==1) strcpy(param,"Yes"); else strcpy (param,"No");
}

//==================以下為SymbolTable==================

//創造一個table，並建立table的記憶體空間
void create(){                                                  
        int i,j;
        for(i=0;i<256;i++){
            for(j=0;j<20;j++){
                Table[i].name[j]=(char)malloc(sizeof(char));
                Table[i].type[j]=(char)malloc(sizeof(char));
                Table[i].scope[j]=(char)malloc(sizeof(char));
                Table[i].attrDimen[j]=(char)malloc(sizeof(char));
                Table[i].attrPoint[j]=(char)malloc(sizeof(char));
                Table[i].attrParam[j]=(char)malloc(sizeof(char));
            }
            strcpy(Table[i].name,"");
            strcpy(Table[i].type,"");
            strcpy(Table[i].scope,"");
            strcpy(Table[i].attrDimen,"");
            strcpy(Table[i].attrPoint,"");
            strcpy(Table[i].attrParam,"");
        }
}

//在Table裡尋找單字，用strcmp來尋找
int lookup(char s[20]){                                        
        int i;
        for(i=0;i<TableTop;i++){
                if(strcmp(s,Table[i].name)==0)return i;              //若在第i位找到，則回傳i (0~255)
        }
        return 256;                                             //若找不到，則回傳256
}

//在Table插入一個新的單字
void insert(char s[20]){
        strcpy(Table[TableTop++].name,s);                            
}

//印出整個Table
void dump(){                                                    
        int i;
        printf("\n==========Symbol Table==========\n");
        printf("Type\t\tName\t\tIsDimen?\tIsPoint?\tIsParam?\n");
        printf("================================\n");
        for(i=0;i<TableTop;i++){
                printf("%s\t\t%s\t\t%s\t\t%s\t\t%s\n",Table[i].type,Table[i].name,Table[i].attrDimen,Table[i].attrPoint,Table[i].attrParam);
        }
        printf("================================\n\n");
}