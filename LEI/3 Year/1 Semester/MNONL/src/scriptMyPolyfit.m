% ==============================================
% ||                   DATA                   ||
% ==============================================

x=[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61];
f=[8857716 8929316 8993985 9030355 9035365 8998595 8930990 8874520 8836650 8757705 8680431 8643756 8630430 8633100 8754365 9093470 9355810 9455675 9558250 9661265 9766312 9851362 9911771 9957865 9996232 10023613 10032734 10030031 10019610 10005000 9983218 9960235 9952494 9964675 9991525 10026176 10063945 10108977 10160196 10217828 10289898 10362722 10419631 10458821 10483861 10503330 10522288 10542964 10558177 10568247 10573100 10557560 10514844 10457295 10401062 10358076 10325452 10300300 10283822 10286263 10305564] %dados da amostra

% ==============================================
% ||                 COMMANDS                 ||
% ==============================================

plot(x,f,'or')              % Draw Points
xaux=1:0.1:62;
hold on;

% 1º Degree

[p1,s1]=polyfit(x,f,1);
(s1.normr)^2;                % Avaluate Module
yaux=polyval(p1,xaux);
plot(xaux,yaux,'b')

% 2º Degree

[p2,s2]=polyfit(x,f,2);
(s2.normr)^2;                % Avaluate Module
hold on;
yaux=polyval(p2,xaux);
plot(xaux,yaux,'g')

% 3º Degree

[p3,s3]=polyfit(x,f,3);
(s3.normr)^2;                % Avaluate Module
hold on;
yaux=polyval(p3,xaux);
plot(xaux,yaux,'r')

% 4º Degree

[p4,s4]=polyfit(x,f,4);
(s4.normr)^2;                % Avaluate Module
hold on;
yaux=polyval(p4,xaux);
plot(xaux,yaux,'y')

% 5º Degree

[p5,s5]=polyfit(x,f,5);
(s5.normr)^2;                % Avaluate Module
hold on;
yaux=polyval(p5,xaux);
plot(xaux,yaux,'k')

% 6º Degree
[p6,s6]=polyfit(x,f,6);
(s6.normr)^2;                % Avaluate Module
hold on;
yaux=polyval(p6,xaux);
plot(xaux,yaux,'m')