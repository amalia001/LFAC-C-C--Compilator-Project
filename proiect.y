%{

#include <stdio.h>
#include <string.h>
extern FILE* yyin;
extern char* yytext;
extern int yylineno;
void yyerror( char* s);
int yylex();
FILE* symbol_table;
FILE* symbol_table_funct;

    int count=0, count_functie=0;
    int params, okfor, okif, okwhile, okdo, oktrue, okfalse, okstruct, okenum, okprint, okreturn, okelse;
    char param[50];

%}
%token INT FLOAT STRING CHAR BOOL NUMBER FLOAT_NUM ID UNARY BGIN END PRINT RETURN VOID FOR ELSE IF WHILE DO LE GE EQ NE GT LT AND OR NOT ADD SUBTRACT DIVIDE MULTIPLY ASSIGN TRUE FALSE CHARACTER STRUCT ENUM DEFINE STR CONST

%union {
char* id;
char* tip;
}
%type <tip> tip_data
%type <id> ID
%start progr
%%

progr:  constante declaratii bloc {printf("program corect sintactic\n");}
     ;

constante: constanta
         | constante constanta
         ; 

constanta: DEFINE ID valoare {fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Constanta", $2, " ", yytext,  " ", yylineno);}
         | CONST tip_data ID ASSIGN valoare {fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Constanta", $3, $2, yytext, " ", yylineno);} ';'
         | tip_data CONST ID ASSIGN valoare {fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Constanta", $3, $1, yytext, " ", yylineno);} ';'
         ;

tip_data: INT  {$$=strdup("int"); }
        | FLOAT {$$=strdup("float"); }
        | STRING {$$=strdup("string"); }
        | CHAR {$$=strdup("char");}
        | BOOL {$$=strdup("bool");}
        | VOID {$$=strdup("void");}
        ;

declaratii: declaratie 
          | declaratii declaratie
          ;

declaratie: tip_data ID { fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Variabila", $2, $1, " ", "Global", yylineno);} ';'
          | tip_data ID '(' lista_param ')' {fprintf(symbol_table_funct, "%-10s %-15s %-30s %-20d %-30s %-25d \n", "Functie", $2, $1, params, param, yylineno); strcpy(param, ""); params=0;} ';'
          | tip_data ID '(' ')' {fprintf(symbol_table_funct, "%-10s %-15s %-30s %-20s %-30s %-25d \n", "Functie", $2, $1, "nu are parametri", " ", yylineno);} ';'
          | tip_data ID ASSIGN valoare { fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Variabila", $2, $1, yytext, "Global", yylineno);} ';'
          | tip_data ID  vector { fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Variabila", $2, $1, " ", "Global", yylineno);}';'
          | tip_data ID matrice { fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Variabila", $2, $1, " ", "Global", yylineno);}';'
          | tip_data ID '(' lista_param ')' {fprintf(symbol_table_funct, "%-10s %-15s %-30s %-20d %-30s %-25d \n", "Functie", $2, $1, params, param, yylineno); strcpy(param, ""); params=0;} '{'declaratii_functie instructiuni'}'  
          | tip_data ID '(' ')' {fprintf(symbol_table_funct, "%-10s %-15s %-30s %-20s %-30s %-25d \n", "Functie", $2, $1, "nu are parametri", " ", yylineno);}'{'declaratii_functie instructiuni'}' 
          ;

declaratii_functie: declaratie_functie ';'
                  | declaratii_functie declaratie_functie ';'
                  ;

declaratie_functie: tip_data ID { fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Variabila", $2, $1, " ", "Local", yylineno);}
                  | tip_data ID ASSIGN valoare { fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Variabila", $2, $1, yytext, "Local", yylineno);}
                  | tip_data ID  vector { fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Variabila", $2, $1, " ", "Local", yylineno);}
                  | tip_data ID matrice { fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Variabila", $2, $1, " ", "Local", yylineno);}
                  ;

lista_param: param 
           | lista_param ',' param 
           ;

param: tip_data ID {params++; strcat(param, $1); strcat(param, " "); strcat(param, $2); strcat(param, " ");}
     ;

vector: '['NUMBER']'
      | '['NUMBER']' ASSIGN '{'lista'}'
      ;

matrice: '['NUMBER']''['NUMBER']'
       | '['NUMBER']' '['NUMBER']' ASSIGN '{'lista'}'
       | '['NUMBER']' '[' ']' ASSIGN '{'lista'}'
       ;

lista: lista ',' list
     | list
     ;

list: NUMBER
    | FLOAT_NUM
    ;

bloc: BGIN '{' instructiuni '}' END  
    ;

instructiuni: instructiune
            | instructiuni instructiune
            ;

instructiune: FOR {if(okfor==0) {fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Cuvant rezervat", "for", " ", " ", " ", yylineno);} okfor=1;}'('expresie ';' conditie ';' expresie ')' '{' instructiuni '}'  //K=KEYWORD
            | IF {if(okif==0) {fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Cuvant rezervat", "if", " ", " ", " ", yylineno);} okif=1;}'('conditie')'  dupa_if
            | WHILE {if(okwhile==0) {fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Cuvant rezervat", "while", " ", " ", " ", yylineno);} okwhile=1;}'(' conditie ')' do 
            | initializare
            | STRUCT  {if(okstruct==0) {fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Cuvant rezervat", "struct", " ", " ", " ", yylineno);} okstruct=1;} ID '{' declaratii_struct '}' dupa_struct
            | ENUM  {if(okenum==0) {fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Cuvant rezervat", "enum", " ", " ", " ", yylineno);} okenum=1;} ID '{'enumeratii'}' ';'
            | PRINT  {if(okprint==0) {fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Cuvant rezervat", "print", " ", " ", " ", yylineno);} okprint=1;}'(' STR ',' formule ')' ';'
            | return
            ;

declaratii_struct: declaratie_struct ';'
                 | declaratii_struct  declaratie_struct ';' 
                 ;

declaratie_struct: tip_data ID { fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Membru struct", $2, $1, " ", "In struct", yylineno);}
                 | tip_data ID vector   { fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Membru struct", $2, $1, " ", "In struct", yylineno);}
                 | tip_data ID matrice  { fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Membru struct", $2, $1, " ", "In struct", yylineno);}
                 ; 

dupa_struct: ID ';'
           | ID '['NUMBER ']' ';'
           | ';'
           ;

dupa_if: '{' instructiuni '}' ELSE {if(okelse==0) {fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Cuvant rezervat", "else", " ", " ", " ", yylineno);} okelse=1;} '{' instructiuni '}'
       |'{' instructiuni '}'
       ;

do: DO {if(okdo==0) {fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Cuvant rezervat", "do", " ", " ", " ", yylineno);} okdo=1;} '{' instructiuni '}'
  ;

enumeratii: enumeratii ',' enumeratie
          | enumeratie
          ;

enumeratie: ID
          | ID ASSIGN NUMBER
          ;

initializare: ID ASSIGN formule';'
            | ID ASSIGN valoare_init ';'
            | ID UNARY ';'
            | UNARY ID ';'
            | functie ';'
            | ID '['NUMBER']' ASSIGN formule';'
            | ID '['NUMBER']' '['NUMBER']' ASSIGN formule';'
            | ID'.'ID ASSIGN formule';'
            | ID '['NUMBER ']' '.'ID ASSIGN formule ';'
            | ID '['ID']' ASSIGN formule';'
            | ID '['ID']' '['NUMBER']' ASSIGN formule';'
            | ID '['ID ']' '.'ID ASSIGN formule ';'            
            ;

functie: ID '('lista_param_funct')'
       ;

lista_param_funct: lista_param_funct ',' list_param_funct
                 | list_param_funct
                 ;

list_param_funct: formule
                | ID '('lista_param_funct')' 
                ;

expresie: ID ASSIGN formule
        | ID UNARY 
        | UNARY ID
        ;

conditie: formule operatori_relat formule
        | TRUE {if(oktrue==0) {fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Cuvant rezervat", "TRUE", " ", " ", " ", yylineno);} oktrue=1;}
        | FALSE {if(okfalse==0) {fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Cuvant rezervat", "FALSE", " ", " ", " ", yylineno);} okfalse=1;}
        | '('conditie ')' OR '('conditie ')'
        | '('conditie ')' AND '('conditie ')'
        | NOT conditie 
        ;

formule: formule operatii formula
       | formula
       | '('formule operatii formula ')'
       ;

formula: valoare_formula
       | ID '['NUMBER']'
       | ID '['NUMBER']' '['NUMBER']'
       | ID'.'ID
       | ID '['NUMBER ']' '.'ID
       | ID '['ID']'
       | ID '['ID']' '['ID']'
       | ID '['ID ']' '.'ID
       ;

operatii: ADD 
        | SUBTRACT 
        | MULTIPLY
        | DIVIDE
        ;

operatori_relat: LT
               | GT
               | LE
               | GE
               | EQ
               | NE
               ;

valoare_init: CHARACTER 
     | STR  
     | TRUE 
     | FALSE
     ;

valoare:NUMBER 
       | FLOAT_NUM
       | ID
       | CHARACTER 
       | STR  
       | TRUE 
       | FALSE
       ;

valoare_formula:NUMBER 
       | FLOAT_NUM
       | ID
       ;

return: RETURN formule {if(okreturn==0) {fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20d \n", "Cuvant rezervat", "RETURN", " ", " ", " ", yylineno);} okreturn=1;} ';' 
      ;

%%
void yyerror(char * s){
  printf("eroare: %s la linia:%d\n",s,yylineno);
}

int main(int argc, char** argv){
    symbol_table = fopen("tabel_simbol.txt","w");
    symbol_table_funct = fopen("tabel_simbol_funct.txt","w");
    fprintf(symbol_table, "%-20s %-20s %-20s %-20s %-20s %-20s \n\n", "TIP", "SIMBOL", "TIP_DE_DATA", "VALOARE", "SCOP", "LINIA");
    fprintf(symbol_table_funct, "%-10s %-15s %-30s %-20s %-30s %-25s \n\n", "TIP", "SIMBOL", "TIP_DE_DATA_RETURNAT", "NR_PARAMETRI", "PARAMETRII SI TIPUL LOR", "LINIA");
  yyin=fopen(argv[1],"r");
  yyparse();
}
