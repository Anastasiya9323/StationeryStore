:- use_module(library(pce)).
:- use_module(library(odbc)).
:- use_module(library(tabular)).
:- use_module(library('plot/barchart')).


create_form:-
    new(D, dialog('Форма для входа')),
    send(D,append, new(NameText,text_item('Имя пользователя'))),
    send(D,append,new(PasswordText,text_item('Пароль'))),
    send(D,append,button('Войти',message(@prolog,login_user,NameText?selection,PasswordText?selection,D))),
    send(D,append,button('Зарегистрироваться',message(@prolog,registration_form,D))),
    send(D,append,button('Выход',message(@prolog,exit,D))),
    send(D,open).

registration_form(Dialog):-
    send(Dialog,destroy),
    new(D, dialog('Регистрация')),
    send(D,append, new(SurnameText,text_item('Фамилия'))),
    send(D,append, new(NameText,text_item('Имя'))),
    send(D,append, new(PatronymicText,text_item('Отчество'))),
    send(D,append,new(BirthdateText,text_item('Дата рождения (гггг-мм-дд)'))),
    send(D,append, new(LoginText,text_item('Имя пользователя'))),
    send(D,append,new(PasswordText,text_item('Пароль'))),
    send(D,append,button('Зарегистрироваться',message(@prolog,create_user,SurnameText?selection,NameText?selection,PatronymicText?selection,BirthdateText?selection,LoginText?selection,PasswordText?selection,D))),
    send(D,append,button('Выход',message(@prolog,exit_userform,D))),
    send(D,open).

create_user(Surname,Name,Patronymic,Birthdate,Login,Password,D):-
    open_connection,
    atom_length(Login,X),
    atom_length(Password,Y),
    atom_length(Patronymic,P),
    atom_length(Birthdate,B),
    atom_length(Surname,S),
    atom_length(Name,N),
    ( (X==0;Y==0;P==0;B==0;S==0;N==0)->
    new(Dialog,dialog('Пустое поле!')),
       send(Dialog,append,new(Label,label)),
       send(Label,value,'Заполните все поля!'),
       send(Dialog,open);
    ((X<5;Y<5) ->
    new(Dialog,dialog('Некорректные значения!')),
       send(Dialog,append,new(Label,label)),
       send(Label,value,'Значения имени пользователя и пароля должны быть не меньше 5 символов'),
       send(Dialog,open);
    insert_datas(Surname,Name,Patronymic,Birthdate,Login,Password),
    send(D,destroy),
    create_form)).

insert_datas(Surname,Name,Patronymic,Birthdate,Login,Password):-
    format(atom(Query), 'INSERT INTO users(surname,name,patronymic,birthdate,login, password, type) VALUES (\'~w\',\'~w\',\'~w\',\'~w\',\'~w\',\'~w\',\'customer\')', [Surname,Name,Patronymic,Birthdate,Login,Password]),
    odbc_query('postgresql', Query).


login_user(NameText,PasswordText,D):-
    open_connection,
    select_datas(NameText,PasswordText,D).

wrong_login:-
    new(Dialog,dialog('Ошибка')),
       send(Dialog,append,new(LabelText,label)),
       send(LabelText,value,'Такого пользователя не существует!'),
       send(Dialog,open).

exit(D):-
   send(D,destroy).

exit_userform(D):-
   send(D,destroy),
   create_form.


open_connection:-
   odbc_connect('PostgreSQL30', _,
 [ user(postgres),
 password('d2qn5bqcl'),
 alias(postgresql),
 open(once)
 ]).

close_connection:-
    odbc_disconnect('postgresql').

select_datas(UserName,UserPassword,D):-
   format(atom(QueryName), 'SELECT type FROM users WHERE login=\'~w\' AND password=\'~w\'', [UserName,UserPassword]),
   (odbc_query('postgresql',QueryName,row(Type)) -> nl;wrong_login,!, fail),
   odbc_query('postgresql',
               QueryName,row(Type)),
    send(D,destroy),
   (Type=='admin' -> open_adminform; open_userform).

open_adminform:-
    new(Dialog, dialog('Окно для администратора')),
   % send(D, size, size(500, 400)),
%open_connection,
    send(Dialog,append, tab_stack(new(Tab1, tab("Добавление товара")),
                                  new(Tab2, tab("Изменение товара")),
                                  new(Tab3, tab("Удаление товара")),
                                  new(Tab4,tab("Гистограмма"))
                                 )),
    add_table(Tab1,'admin',ListName,ListQuantity,ListPrice),
    add_table(Tab2,'admin',ListName,ListQuantity,ListPrice),
    add_table(Tab3,'admin',ListName,ListQuantity,ListPrice),
    send(Tab1,append,button('Добавить товар',message(@prolog,add_product,Dialog))),
    send(Tab2,append,button('Изменить товар',message(@prolog,change_product,Dialog,prolog(ListName)))),
    send(Tab3,append,button('Удалить товар',message(@prolog,delete_product,Dialog,prolog(ListName)))),
    merge_lists(ListName,ListQuantity,Res),
    merge_lists(Res,ListPrice,ResP),
    new(BC, bar_chart(vertical,0,450,200,20)),
    forall(member(Name/Quantity/Price,
              ResP),
           send(BC,append,bar_group(Name,bar(количество,Quantity,green),bar(цена,Price,blue)))),
    send(Tab4,append,BC),
    send(Dialog,append,button('Выход',message(@prolog,exit_userform,Dialog))),
    send(Dialog, open),
    close_connection.

merge_lists([], [], []).
merge_lists([N|Ns], [Q|Qs], [N/Q|Result]) :-
    merge_lists(Ns, Qs, Result).

car([H|_],H).
del(X, 0, X).
del([_|X],1,X).
reverse(List, ReverseList):-
   reverse(List, [], ReverseList).
reverse([], Buffer, Buffer):-!.
reverse([Head|Tail], Buffer, ReverseList):-
   reverse(Tail, [Head|Buffer], ReverseList).

open_userform:-
    new(Dialog, dialog('Каталог канцтоваров')),
    add_table(Dialog,'user',ListName,ListQuantity,ListPrice),
    del(ListName,0,ListName),
    del(ListQuantity,0,ListQuantity),
    del(ListPrice,0,ListPrice),
    send(Dialog,append,button('Выход',message(@prolog,exit_userform,Dialog))),
    send(Dialog, open),
    close_connection.

add_table(D,User,ListName,ListQuantity,ListPrice):-
    send(D, append, new(T, tabular)),
    send(T, border, 1),
    send(T, cell_spacing, -1),
    send(T, rules, all),
    XName=[],
    XArticle=[],
    XQuantity=[],
    XPrice=[],
    fetch(ListName1,ListArticle1,ListQuantity1,ListPrice1,XName,XArticle,XQuantity,XPrice,1,next),
    reverse(ListName1,ListName),
    reverse(ListArticle1,ListArticle),
    reverse(ListQuantity1,ListQuantity),
    reverse(ListPrice1,ListPrice),
    send_list(T,
              [
                  %append(new(graphical), rowspan := 1),
                  append('Каталог', bold, center, colspan := 4),
                  next_row,
                  append('Наименование товара', bold, center),
                  append('Артикул', bold, center),
                  append('Количество', bold, center),
                  append('Цена', bold, center)
              ]),
    add_to_table(T,ListName,ListArticle,ListQuantity,ListPrice,User).

add_to_table(T,ListName,ListArticle,ListQuantity,ListPrice,User):-
    (   User=='user' ->
    (   ListName==[]->true;
    car(ListName,X),
        car(ListArticle,Y),
        car(ListQuantity,Z),
        car(ListPrice,W),
        ( Z>0 ->
        send(T,next_row),
          send(T,append(X)),
          send(T,append(Y)),
          send(T,append(Z)),
          send(T,append(W));nl),
        del(ListName,1,ListName1),
        del(ListArticle,1,ListArticle1),
        del(ListQuantity,1,ListQuantity1),
        del(ListPrice,1,ListPrice1),
        add_to_table(T,ListName1,ListArticle1,ListQuantity1,ListPrice1,User));

    (   ListName==[]->true;
    car(ListName,X),
    car(ListArticle,Y),
    car(ListQuantity,Z),
    car(ListPrice,W),
    send(T,next_row),
    send(T,append(X)),
    send(T,append(Y)),
    send(T,append(Z)),
    send(T,append(W)),
    del(ListName,1,ListName1),
    del(ListArticle,1,ListArticle1),
    del(ListQuantity,1,ListQuantity1),
    del(ListPrice,1,ListPrice1),
    add_to_table(T,ListName1,ListArticle1,ListQuantity1,ListPrice1,User)))
    .

add_to_list(Element, List, NewList) :-
    NewList = [Element|List].

fetch(ListName,ListArticle,ListQuantity,ListPrice,XName,XArticle,XQuantity,XPrice,Count,Options) :-
        odbc_set_connection('postgresql', encoding(locale)),%cursor_type(static)),
        odbc_prepare('postgresql',
                     'select * from products',
                     [],
                     Statement,
                     [ fetch(fetch)
                     ]),
        odbc_execute(Statement, []),
        fetch(ListName,ListArticle,ListQuantity,ListPrice,XName,XArticle,XQuantity,XPrice,Count,Statement, Options).

fetch(ListName,ListArticle,ListQuantity,ListPrice,XName,XArticle,XQuantity,XPrice,Count,Statement, Options) :-
        odbc_fetch(Statement, Row, Options),
        (   Row == end_of_file
        ->
        add_to_list(Name,XName,XName1),
            add_to_list(Article,XArticle,XArticle1),
            add_to_list(Quantity,XQuantity,XQuantity1),
            add_to_list(Price,XPrice,XPrice1),
            ListName=XName,
            ListArticle=XArticle,
            ListQuantity=XQuantity,
            ListPrice=XPrice,
            true
        ;    format(atom(QueryName),'SELECT productname
FROM (
    SELECT *, ROW_NUMBER() OVER (ORDER BY id) AS rownum
    FROM products
) AS sub
WHERE rownum =\'~d\'', [Count]),
    odbc_query('postgresql',
               QueryName,row(Name)),
    format(atom(QueryArticle), 'SELECT articlenumber
FROM (
    SELECT *, ROW_NUMBER() OVER (ORDER BY id) AS rownum
    FROM products
) AS sub
WHERE rownum =\'~d\'', [Count]),
    odbc_query('postgresql',
               QueryArticle,row(Article)),
    format(atom(QueryQuantity), 'SELECT quantity
FROM (
    SELECT *, ROW_NUMBER() OVER (ORDER BY id) AS rownum
    FROM products
) AS sub
WHERE rownum =\'~d\'', [Count]),
    odbc_query('postgresql',
               QueryQuantity,row(Quantity)),
    format(atom(QueryPrice), 'SELECT price
FROM (
    SELECT *, ROW_NUMBER() OVER (ORDER BY id) AS rownum
    FROM products
) AS sub
WHERE rownum =\'~d\'', [Count]),
    odbc_query('postgresql',
               QueryPrice,row(Price)),

        add_to_list(Name,XName,XName1),
            add_to_list(Article,XArticle,XArticle1),
            add_to_list(Quantity,XQuantity,XQuantity1),
            add_to_list(Price,XPrice,XPrice1),
            fetch(ListName,ListArticle,ListQuantity,ListPrice,XName1,XArticle1,XQuantity1,XPrice1,Count+1,Statement, Options)
        ).

add_product(D):-
       new(Dialog, dialog('Добавление нового товара')),
       send(Dialog,append, new(NameText,text_item('Наименование товара'))),
       send(Dialog,append, new(ArticleText,text_item('Артикул'))),
       send(Dialog,append, new(QuantityText,int_item('Количество'))),
       send(Dialog,append, new(PriceText,int_item('Цена'))),
       send(Dialog,append,button('Добавить',message(@prolog,add,NameText?selection,ArticleText?selection,QuantityText?selection,PriceText?selection,Dialog,D))),
       send(Dialog, open).

add(Name,Article,Quantity,Price,Dialog,D):-
    open_connection,
     format(atom(Query), 'INSERT INTO products(productname, articlenumber, quantity, price) VALUES (\'~w\',\'~w\', ~d, ~d)', [Name, Article, Quantity, Price]),
    odbc_query('postgresql', Query),
    send(Dialog,destroy),
    send(D,destroy),
    open_adminform.

delete_product(D,ListName):-
       new(Dialog, dialog('Удаление товара')),
       send(Dialog,append,new(NamePopup, menu('Наименование товара', cycle))),
       send_list(NamePopup, append, ListName),
       send(Dialog,append,button('Удалить',message(@prolog,delete,NamePopup?selection,Dialog,D))),
       send(Dialog, open).

delete(Name,Dialog,D):-
    open_connection,
     format(atom(Query), 'delete from products where productname=(\'~w\')', [Name]),
    odbc_query('postgresql', Query),
    send(Dialog,destroy),
    send(D,destroy),
    open_adminform.

change_product(D,ListName):-
       new(Dialog, dialog('Изменение товара')),
       send(Dialog,append,new(NamePopup, menu('Наименование товара', cycle))),
       send_list(NamePopup, append, ListName),
       send(Dialog,append,button('Выбрать товар для изменения',message(@prolog,choose_product,NamePopup?selection,Dialog,D))),
       send(Dialog, open).

choose_product(NamePopup,D,D1):-
    open_connection,
    format(atom(QueryArticle),'SELECT articlenumber FROM products WHERE productname=\'~w\'',[NamePopup]),
    odbc_query('postgresql', QueryArticle,row(ArticleVal)),
    format(atom(QueryQuantity),'SELECT quantity FROM products WHERE productname=\'~w\'',[NamePopup]),
    odbc_query('postgresql', QueryQuantity,row(QuantityVal)),
format(atom(QueryPrice),'SELECT price FROM products WHERE productname=\'~w\'',[NamePopup]),
    odbc_query('postgresql', QueryPrice,row(PriceVal)),
       new(Dialog, dialog('Изменение товара')),
       send(Dialog,append,new(NameLabel, label)),
       format(atom(Text),'Текущее наименование товара: ~w',[NamePopup]),
       send(NameLabel,value,Text),
       send(Dialog,append,new(Name,text_item('Наименование товара'))),
       send(Dialog,append,new(Article,text_item('Артикул'))),
       send(Dialog,append,new(Quantity,int_item('Количество'))),
       send(Dialog,append,new(Price,int_item('Цена'))),
       send(Name,value,NamePopup),
       send(Article,value,ArticleVal),
       send(Quantity,value,QuantityVal),
       send(Price,value,PriceVal),
       send(Dialog,append,button('Изменить',message(@prolog,change,NamePopup,Name?selection,Article?selection,Quantity?selection,Price?selection,Dialog,D,D1))),
       send(Dialog, open).

change(NamePopup,Name,Article,Quantity,Price,Dialog,D,D1):-
    open_connection,
    format(atom(Query),'UPDATE products set productname=\'~w\',articlenumber = \'~w\',quantity=\'~d\',price=\'~d\' where productname=\'~w\'',[Name,Article,Quantity,Price,NamePopup]),
    odbc_query('postgresql', Query),
    send(Dialog,destroy),
    send(D,destroy),
    send(D1,destroy),
    open_adminform.

