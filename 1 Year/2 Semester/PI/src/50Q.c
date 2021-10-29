#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <stdbool.h>
#include "../includes/view.h"

#define STRING_SIZE 1000
#define MAX 100

//---------------------------------------------------------------------------------------------------------------------------------------------

int f1 () {
    int maior = INT_MIN; // Defines the minimum value for an int
    int numero = 0;

    puts("Funcao 1 -> Vai-se inserir uma sequencia de numeros acabada em 0 e determinar o maior.");

    do {
        puts("Insert number, 0 to exit");
        scanf("%d", &numero);

        if (maior < numero) maior = numero;
    } while(numero != 0);

    printf("Biggest boy: %d\n", maior);
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f2 () {
   int numero,total=0,soma=0;

   puts("Funcao 2 -> Vai-se inserir uma sequencia de numeros acabada em 0 e determinar a media.");

   do{
     puts("Insert number, 0 to exit");
     scanf("%d",&numero);
     total+=1;
     soma+=numero;
   }while(numero!=0);

   printf("A media e: %d\n",soma/total);
   return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f3 () {
  int numero;
  int maior = INT_MIN; // Defines the minimum value for an int
  int segMaior = INT_MIN; // Defines the minimum value for an int

  puts("Funcao 3 -> Vai-se inserir uma sequencia de numeros acabada em 0 e determinar o segundo maior.");

  do{
    puts("Insert number, 0 to exit");
    scanf("%d",&numero);

    if (maior < numero){
        segMaior = maior;
        maior = numero;
    }
  }while(numero!=0);

  printf("Second Big Boy: %d\n",segMaior);
  return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f4 () {
    unsigned int numero;
    int res=0,original;

    puts("Funcao 4 -> Funcao bitsUm, ve quantos 1s o numero tem em binario. Insira o numero:");
    scanf("%d",&numero);
    original = numero;

    while(numero>0){
        if(numero%2!=0){
            res+=1;
            numero/=2;
        }
    }

    printf("O numero inserido foi %d, e na sua representacao binaria existem %d 1s.\n",original,res);
    return res;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f5 () {
    unsigned int numero;
    int original, res=0;

    puts("Funcao 5 -> Funcao Trailingz, calcula o numero de bits a 0 no final da representacao binaria. Insira o seu numero:");
    scanf("%d",&numero);
    original = numero;

    while(numero>0){
        if(numero%2==0) res+=1;
        else {break;}
        numero/=2;
    }

    printf("O numero inserido foi %d, e na sua representacao binaria existem %d 0s.\n",original,res);
    return res;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f6 () {
    int numero, original, res=0;

    puts("Funcao 6 -> Funcao qDig, diz quantos digitos o numero tem. Insira o seu numero:");
    scanf("%d",&numero);
    original = numero;

    while (numero > 0) {
        res+=1; 
        numero/=10;
    }

    printf("O numero inserido foi %d, e o seu numero de digitos e %d.\n", original, res);
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f7 () {
    char primeira[STRING_SIZE], segunda[STRING_SIZE], original[STRING_SIZE];
    int i, j;

    puts("Funcao 7 -> Funcao strcat, Concatena duas strings e muda a tua vida para sempre!");
    
    puts("\t Digita e primeira String:");
    scanf("%s",&primeira);    
    strcpy(original, primeira);

    printf("\t Digita a segunda String:");
    scanf("%s",&segunda);

    for (i = 0; primeira[i]; i++);
    for (j = 0; segunda[j]; j++, i++){
        primeira[i] = segunda[j];
    }

    primeira[i] = '\0';

    printf("Concatenar %s com %s da: %s\n", original, segunda, primeira);
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f8 () {
    char original[STRING_SIZE], copia[STRING_SIZE];
    int i;

    puts("Funcao 8 -> Funcao strcpy, copia uma string de um lado para o outro! Digite a cena que quer copiar:");
    scanf("%s",&original);

    for (i = 0; original[i]; i++){
        copia[i] = original[i];
    }

    copia[i] = '\0';

    printf("O original e %s e a copia e %s. Deve ser igual.\n", original, copia);
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f9 () {
    char primeira[STRING_SIZE], segunda[STRING_SIZE];
    int i, res;

    puts("Funcao 9 -> Funcao strcmp, ve se duas strings sao iguais. Digite a primeira:");
    scanf("%s", primeira);
    puts("Bem jogado, agora a segunda:");
    scanf("%s", segunda);

    for (i = 0; primeira[i] == segunda[i] && primeira[i]; i++);

    res = primeira[i] - segunda[i];

    if (res < 0)
        printf("%s vem antes de %s no dicionario.\n", primeira, segunda);
    else if (!res)
        printf("%s e %s sao iguais, weeee.\n", primeira, segunda);
    else
        printf("%s vem antes de %s no dicionario.\n", segunda, primeira);

    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f10 () {
    int i = 0, j = 0, contida = 1;
    char haystack[STRING_SIZE], needle[STRING_SIZE];
    char* res = haystack; 

    puts("Funcao 10 -> Funcao strstr, ve se a segunda string aparece na primeira e imprime a partir dai. Digite a primeira string:");
    scanf("%s", haystack);

    puts("Agora digite a segunda:");
    scanf("%s", needle);

    while(needle[i] && haystack[i]){
        if(haystack[j]!=needle[i]) contida = i = 0;
        if(haystack[j]==needle[i]){
            if(!contida){
                contida = 1;
                res = haystack + j;
            }
            i++;
        }
        j++;
    }
    if(contida && !needle[i]){
       printf("A string %s nao aparece na string %s. Temos pena.\n", needle, haystack);
       return res;
    }else{
        printf("A string %s aparece na string %s a partir de: %s\n", needle, haystack, res);
        return NULL;
    }
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f11 () {
    char s[STRING_SIZE],original[STRING_SIZE];

    puts("Funcao 11 -> Funcao strrev, Inverte a string inserida. Insira uma string para inverter e tal:");
    scanf("%s", &s);
    strcpy(original, s);

    int i;
    for (i = 0; s[i] ; i++);
    i--;
    for(int j = 0; j < i ; j++, i--) {
        char temp = s[j];
        s[j] = s[i];
        s[i] = temp;
    }

    printf("A palavra era %s e passou a %s\n", original, s); 
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int removeIndex(char s[],int n){ //remove o char de um dado index
    for(int j = n; s[j] ; j++){ 
        s[j]=s[j+1];
    }
}

int f12 () {
    int  i = 0;
    char string[STRING_SIZE];
    char c;

    puts("Funcao 12 -> Funcao strnoV, retira todas a vogais de uma string. Insira a string:"); 
    scanf ("%s", &string);

    while(c=string[i]){
        if (c=='A' || c=='E' || c=='I' || c=='O' || c=='U' || c=='a' || c=='e' || c=='i' || c=='o' || c=='u') removeIndex(string,i);
        else i+=1;
    }
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f13 () {
    int num,wrdlen,i=0;
    char c;
    char string[STRING_SIZE],original[STRING_SIZE];

    puts("Funcao 13 -> Funcao truncW, que dado um texto com varias palavras (as palavras estao separadas por um ou mais espacos) e um inteiro , trunca todas as palavras de forma a terem no maximo n caracteres.");
    puts("Insere a string:");
    scanf("%s",&string);
    strcpy(original,string);
    puts("Insere o inteiro:");
    scanf("%i",&num);

    while(c==string[i]){
        if(c==' ' || c=='\n' || c=='\t'){
            i+=1;
            wrdlen=0;
        }else{
           if(wrdlen++ >= num) removeIndex(string,i);
           else i++;
        }
    }

    printf("A tua string original era %s , depois da funcao truncw ficou %s\n",original,string);
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f14 () {
    char string[STRING_SIZE];
    int charFreq[256];
    int freqMax = 0;
    int i;

    puts("Funcao 14 -> Funcao charMaisfreq, que determina qual o caracter mais frequente numa string. Insere a String:");
    scanf("%s",&string);

    char maisFreq = string[0];

    if(!string[0]) return 0; // funciona como uma pergunta, se string[0] for verdade então retorna 0 se for mentira segue para a frente

    for(i=0;i<256;i++) charFreq[i]=0;

    for(i=0;string[i]!='\0';i++){
        int c=string[i];
        charFreq[c]+=1;
        if(charFreq[c] > freqMax){
          freqMax = charFreq[c];
          maisFreq = string[i];
        }
    }
    printf("O char que ocorre mais vezes na string %s e o %c",string,maisFreq);
    return 0; // pelo enunciado mete-se: return maisFreq
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f15 (){
    int i,consec=1,max=0;
    char string[STRING_SIZE];

    puts("Funcao 15 -> Funcao iguaisConsecutivos, dada uma string calcula o comprimento da maior sub-string com caracteres iguais. Insere a string:");
    scanf ("%s",&string);

     for(i = 0; string[i]!='\0'; i++) {
        if(string[i] == string[i + 1]) 
            consec++;
        else {
            if(consec > max) 
                max = consec;
            consec = 1;
        }
    }
    printf("O comprimento da maior sub-string da string %s e %i\n",string,consec);
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int aux (char str[],int k,int n){
    int i, ans=1;
    for(i=0;i<n;i++){
        if(str[i]==str[n]){
          ans = 0;
          break;
        }
    }
    return ans;
}

int f16 (){
    int i,j,ans=0,consec=0;
    char string[STRING_SIZE];

    puts("Funcao 16 -> Funcao difConsecutivos, que, dada uma string calcula o  comprimento da maior sub-string com caracteres diferentes. Insere a string:");
    scanf("%s",&string);

    for(i=0;string[i]!='\0';i++){
        for(j=i;string[j]!='\0';j++){
            if(aux(string,i,j)) consec++;
            else break;
        }
        if(consec>ans) ans=consec;
    }

    printf("O comprimento da maior sub-string, com caracteres diferentes, da string %s e %i\n",string,ans);
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f17 (){
    char s1[STRING_SIZE],s2[STRING_SIZE];
    int rez=0;

    puts("Funcao 17 -> Funcao maiorPrefixo, calcula o comprimento do maior prefixo comum entre as duas strings. Insere a primeira String:");
    scanf("%s",&s1);
    puts("Bem jogado, agora insere a segunda String:");
    scanf("%s",&s2);

    for(int i=0 ; s1[i]==s2[i] && s1[i] ; i++) rez++;

    printf("O comprimento do maior prefixo comum as duas string inseridas e : %d\n",rez);
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f18 (){
    char s1[STRING_SIZE],s2[STRING_SIZE];
    int i,j,rez=-1;

    puts("Funcao 18 -> Funcao maiorSufixo, calcula o comprimento do maior sufixo comum entre as duas strings. Insere a primeira String:");
    scanf("%s",&s1);
    puts("Bem jogado, agora insere a segunda String:");
    scanf("%s",&s2);

    for(i=0 ; s1[i] ; i++);
    for(j=0 ; s2[j] ; j++);
    while (s1[i--] == s2[j--]) rez++;
    
    printf("O comprimento do maior sufico comum as duas strings inseridas e : %d\n",rez);
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f19 () {
    char s1[STRING_SIZE],s2[STRING_SIZE];
    int i,j=0,rez=0;

    puts("Funcao 19 -> Funcao sufPref, que calcula o tamanho do maior sufixo de s1 que e um prefixo de s2. Insere a primeira String:");
    scanf("%s",&s1);
    puts("Bem jogado, agora insere a segunda String:");
    scanf("%s",&s2);

    for(i=0 ; s1[i] ; i++){
        if(s1[i] == s2[j++]) rez++;
        else rez=j=0;
    }
    
    printf("O tamanho do BIG-BOY sufixo de s1 e s2 e: %d\n",rez);
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f20 () {
    char s[STRING_SIZE];

    puts("Funcao 20 -> Funcao contaPal, que conta as palavras de uma string. Uma palavra e uma sequencia de caracteres (diferentes de espaco) terminada por um ou mais espacos. Insere a String:");
    scanf("%s",&s);

    int inWord=0, count=0;
    for(int i = 0; s[i]; i++) {
        if(inWord && (s[i] == ' ' || s[i] == '\n'))
            inWord = 0;
        else
            if(!inWord && s[i] != ' ' && s[i] != '\n') {
                inWord = 1;
                count ++;
            }
    }

    printf("O numero de palavras contidas na String e igual a %i\n",count);
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

bool isVogal (char c) {
    int res = false;
    if (c=='A' || c=='E' || c=='I' || c=='O' || c=='U' || c=='a' || c=='e' || c=='i' || c=='o' || c=='u')
        res = true; 
    return res;
}

int f21 () {
    char s[STRING_SIZE];

    puts("Funcao 21 -> Funcao contaVogais, que conta as vogais de uma String. Insere a String:");
    scanf("%s",&s);

    int count=0;
    for(int i = 0; s[i]; i++)
        if(isVogal(s[i]))
            count ++;
    
    printf("O numero de vogais contidas na String e igual a %i\n",count);
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f22 () {
    char a[STRING_SIZE], b[STRING_SIZE];
    
    puts("Funcao 22 -> Funcao contida, que recebe duas string e verifica se uma esta contida na outra. Insere a primeira String:");
    scanf("%s",&a);
    puts("Bem jogado, agora insere a segunda String:");
    scanf("%s",&b);

    int j, ans = 1;
    for(int i = 0; a[i] && ans; i++) {
        for(j = 0; b[j] && a[i] != b[j]; j++);
        if(!b[j])
            ans = 0;
    }

    if(ans = 0) {
        printf("A String %s esta contida na String %s\n",b,a);
    } else {
        printf("A String %s nao esta contida na String %s\n",b,a);
    }
    return 0; 
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f23 () {
    char s[STRING_SIZE];

    puts("Funcao 23 -> Funcao palindroma, que testa se uma String le-se de igual forma nos dois sentidos. Insere a String:");
    scanf("%s",&s);

    int i = 0, ans = 1;
    for(i; s[i]; i++);
    i--;
    for(int j = 0; j < i && ans; j++, i--)
        ans = s[j] == s[i];
    
    if(ans = 0) {
        printf("A String %s e palindroma\n",s);
    } else {
        printf("A String %s nao e palindroma\n",s);
    }
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f24() {
    char x[STRING_SIZE];
    
    puts("Funcao 24 -> Funcao remRep, que elimina de uma string todos os caracteres que se repetem sucessivamente deixando la apenas uma copia, no final ira retornar o tamanho da nova string. Insere a String:");
    scanf("%s",&x);

    int i;
    for(i = 0; x[i]; i++)
        if(x[i] == x[i + 1]) {
            for(int j = i + 1; x[j]; j++)
                x[j] = x[j + 1];
            i--;
        }
    printf("O tamanho da nova string sera %i\n",i);    
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f25() {
    char t[STRING_SIZE];
    
    puts("Funcao 25 -> Funcao limpaEspacos, funcao que elimina de uma string todos os espacos, no final ira retornar o tamanho da nova string. Insere a String:");
    scanf("%s",&t);

    int i;
    for(i = 0; t[i]; i++)
        if(t[i] == ' ' && t[i + 1] == ' ') {
            for(int j = i + 1; t[j]; j++)
                t[j] = t[j + 1];
            i--;
        }

    printf("O tamanho da nova string sera %i\n",i);    
    return 0; 

}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f26() {
    int v[MAX], N, x;

    puts("Funcao 26 -> Funcao insere, que insere um elemento (x) num vetor ordenado. Quantos elementos tem o vetor?");
    scanf("%i",&N);
    for(int i = 0; i<=N; i++) {
        puts("Insere um numero:");
        scanf("%i",&v[i]);
    }
    puts("Qual e o numero que pretende inserir?");
    scanf("%i",&x);

    int y;
    for(y = 0; y < N && v[y] < x; y++);
    for(; N > y; N--)
        v[N] = v[N - 1];
    v[y] = x;

    puts("O novo vetor e: ");
    for(int j = 0; j <= N; j++)
        print("\t- %i\n",v[j]);
    return 0;
}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f27() {

}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f28() {

}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f29() {

}

//---------------------------------------------------------------------------------------------------------------------------------------------

int f30() {

}

//---------------------------------------------------------------------------------------------------------------------------------------------

int main_loop_50Q() {

    void_fn funcoes[50] = {
      &f1, &f2, &f3, &f4, &f5, &f6, &f7, &f8, &f9, &f10,
      &f11, &f12, &f13, &f14, &f15, &f16, &f17, &f18, &f19, &f20,
      &f21, &f22, &f23, &f24, &f25, &f26, &f27, &f28, &f29, &f30   
    };

    menu(funcoes);

    puts("-----> Program will End <-----");
    return 0;
}