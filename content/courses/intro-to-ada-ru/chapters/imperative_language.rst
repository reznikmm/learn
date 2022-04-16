Императивы языка
================

.. include:: ../../global.txt

Язык Ada ‑ это многопарадигмальный язык с поддержкой объектной
ориентации и некоторых элементов функционального программирования, но
его ядром является простой, последовательный процедурный/императивный
язык, похожий на C или Pascal.

.. admonition:: На других языках

    Одно важное различие между Ada и таким языком, как C, заключается в
    том, что операторы и выражения очень четко различаются. В Ada, если вы
    попытаетесь использовать выражение, в котором требуется оператор, ваша
    программа не сможет скомпилироваться. Это правило поддерживает
    полезный стилистический принцип: выражения предназначены для передачи
    значений, а не для побочных эффектов. Это также может предотвратить
    некоторые ошибки программирования, такие как ошибочное использование
    оператора равенства :ada:`=` вместо операции присваивания :ada:`:=`
    в операторе присваивания.

Hello world
-----------

Вот очень простая императивная программа Ada:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet

    with Ada.Text_IO;

    procedure Greet is
    begin
       --  Print "Hello, World!" to the screen
       Ada.Text_IO.Put_Line ("Hello, World!");
    end Greet;

Текс программы, как мы предполагаем, находится в исходном файле :file:`greet.adb`.

.. only:: builder_html

    Если мы соберем программу с помощью компилятора GNAT и запустим ее, то
    получим ожидаемый результат.

    .. code-block:: sh

        $ gprbuild greet.adb
        using project file [...]_default.gpr
        Compile
           [Ada]          greet.adb
        Bind
           [gprbind]      greet.bexch
           [Ada]          greet.ali
        Link
           [link]         greet.adb

        $ ./greet
        Hello, World!
        $

В вышеупомянутой программе есть несколько примечательных вещей:

-  Подпрограмма в Ada может быть либо процедурой, либо функцией.
   Процедура, как показано выше, не возвращает значение при вызове.

-  :ada:`with` используется для ссылки на внешние модули, которые необходимы в
   процедуре. Это похоже на ``import`` на разных языках или примерно похоже
   на :c:`#include` в C и C++. Позже мы увидим, как они работают в деталях.
   Здесь мы запрашиваем стандартный библиотечный модуль, пакет :ada:`Ada.Text_IO`,
   который содержит процедуру печати текста на экране: :ada:`Put_Line`.

-  :ada:`Greet` ‑ это процедура и основная точка входа в нашу первую программу.
   В отличие от C или C++, его можно назвать как угодно по вашему
   усмотрению. Построитель (builder) определит точку входа. В нашем
   простом примере :program:`gprbuild`, построитель GNAT, будет использовать
   файл, который вы передали в качестве параметра.

-  :ada:`Put_Line` ‑ это такая же процедура, как и :ada:`Greet`, за исключением
   того, что она объявлена в модуле :ada:`Ada.Text_IO`.
   Это Ada-эквивалент :c:`printf` в Си.

-  Комментарии начинаются с :ada:`--`. Комментарии идут до конца строки. Синтаксис
   многострочных комментариев отсутствует, то есть невозможно начать
   комментарий в одной строке и продолжить его в следующей строке.
   Единственный способ создать несколько строк комментариев в Ada ‑ это
   использовать ":ada:`--`" в каждой строке. Например:

.. code-block:: ada

    --  We start a comment in this line...
    --  and we continue on the second line...

.. admonition:: На других языках

    Процедуры аналогичны функциям в C или C++, которые возвращают значение
    :c:`void`. Позже мы увидим, как объявлять функции в Ada.

Вот минорный вариант примера «Hello, World»:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_2

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
    begin
       --  Print "Hello, World!" to the screen
       Put_Line ("Hello, World!");
    end Greet;

В этой версии используется свойство Ada, известное как предложение :ada:`use`,
которое имеет форму :ada:`use` *package-name*. Как показано вызовом на :ada:`Put_Line`,
эффект заключается в том, что на объекты из именованного пакета можно ссылаться
напрямую, без имени пакета – нет необходимости использовать *package-name.*
как префикс.

Императивы языка ‑ If / Then / Else
-----------------------------------

В этом разделе описывается оператор :ada:`if` в Ada, и вводятся некоторые
другие возможности фундаментального языка, включая целочисленный
ввод-вывод, объявления данных и режимы параметров подпрограммы.

Оператор :ada:`if` в Ada довольно неудивителен по форме и функциям:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Check_Positive

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

    procedure Check_Positive is
       N : Integer;
    begin
       --  Put a String
       Put ("Enter an integer value: ");

       --  Read in an integer value
       Get (N);

       if N > 0 then
          --  Put an Integer
          Put (N);
          Put_Line (" is a positive number");
       end if;
    end Check_Positive;

Оператор :ada:`if` минимально состоит из зарезервированного слова :ada:`if`, условия
(которое должно быть логическим значением), зарезервированного слова :ada:`then`
и непустой последовательности операторов (часть :ada:`then`), которая
выполняется, если условие оценивается как True, и завершающий :ada:`end if`.

В этом примере объявляется целочисленная переменная N, запрашивается у
пользователя целое число, проверяется, является ли значение
положительным, и, если да, отображается значение целого числа, за
которым следует строка "является положительным числом" (is a positive
number). Если значение не является положительным, то процедура не
выводит никаких выходных данных.

Тип Integer является предопределенным типом со знаком, и его диапазон
зависит от архитектуры компьютера. На типичных современных процессорах
целое число имеет 32-разрядный знак.

Пример иллюстрирует некоторые основные функциональные возможности для
целочисленного ввода-вывода. Соответствующие подпрограммы находятся в
предопределенном пакете :ada:`Ada.Integer_Text_IO` и включают процедуру
:ada:`Get` (которая считывает число с клавиатуры) и процедуру :ada:`Put`
(которая отображает целое значение).

Вот небольшое изменение в примере, которое иллюстрирует оператор :ada:`if` с
частью :ada:`else`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Check_Positive_2

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

    procedure Check_Positive is
       N : Integer;
    begin
       --  Put a String
       Put ("Enter an integer value: ");

       --  Reads in an integer value
       Get (N);

       --  Put an Integer
       Put (N);

       if N > 0 then
          Put_Line (" is a positive number");
       else
          Put_Line (" is not a positive number");
       end if;
    end Check_Positive;

В этом примере, если входное значение не является положительным, то
программа отображает значение, за которым следует строка "не является
положительным числом" (is not a positive number).

Наш последний вариант иллюстрирует оператор :ada:`if` с разделами :ada:`elsif`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Check_Direction

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

    procedure Check_Direction is
       N : Integer;
    begin
       Put ("Enter an integer value: ");
       Get (N);
       Put (N);

       if N = 0 or N = 360 then
          Put_Line (" is due north");
       elsif N in 1 .. 89 then
          Put_Line (" is in the northeast quadrant");
       elsif N = 90 then
          Put_Line (" is due east");
       elsif N in 91 .. 179 then
          Put_Line (" is in the southeast quadrant");
       elsif N = 180 then
          Put_Line (" is due south");
       elsif N in 181 .. 269 then
          Put_Line (" is in the southwest quadrant");
       elsif N = 270 then
          Put_Line (" is due west");
       elsif N in 271 .. 359 then
          Put_Line (" is in the northwest quadrant");
       else
          Put_Line (" is not in the range 0..360");
       end if;
    end Check_Direction;

В этом примере ожидается, что пользователь введет целое число от 0 до
360 включительно, и отобразит, какому квадранту или оси соответствует
значение. Оператор :ada:`in` в Ada проверяет, находится ли скалярное значение в
указанном диапазоне, и возвращает логический результат. Эффект
программы должен быть очевиден; позже мы увидим альтернативный и более
эффективный стиль для достижения того же эффекта используя оператор :ada:`case`.

Ключевое слово :ada:`elsif` в Ada отличается от C или C ++, где вместо него будут
использоваться блоки :ada:`else .. if`. И еще одно отличие ‑ это наличие конца
:ada:`end if` в Аде, что позволяет избежать проблемы, известной как «dangling else»
(неоконченное else).

Императивы языка ‑ Loops
------------------------

У Ada есть три способа задания циклов. Однако они отличаются от цикла
for в C / Java / Javascript. Цикл for-loop обладает более простым
синтаксисом и семантикой в соответствии с философией Ada.

Циклы For
~~~~~~~~~

Первый вид цикла ‑ это цикл :ada:`for`, который позволяет выполнять итерацию
через дискретный диапазон.

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_5a

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet_5a is
    begin
       for I in 1 .. 5 loop
          --  Put_Line is a procedure call
          Put_Line ("Hello, World!" & Integer'Image (I));
          --        ^ Procedure parameter
       end loop;
    end Greet_5a;

.. only:: builder_html

    Исполнение этой процедуры дает следующий результат:

    .. code-block:: sh

       Hello, World! 1
       Hello, World! 2
       Hello, World! 3
       Hello, World! 4
       Hello, World! 5

Несколько вещей, которые следует отметить:

-  :ada:`1 .. 5` ‑ это дискретный диапазон, от :ada:`1` до :ada:`5` включительно.

-  Параметр цикла :ada:`I` (имя произвольное) в теле цикла имеет значение в этом
   диапазоне.

-  :ada:`I` является локальным для цикла, поэтому вы не можете ссылаться на :ada:`I`
   вне цикла.

-  Хотя значение :ada:`I` увеличивается на каждой итерации, с точки зрения
   программы оно является постоянным. Попытка изменить его значение
   является незаконной; компилятор отклонит это действие в программе.

-  :ada:`Integer'Image` ‑ это функция, которая принимает целое число и преобразует
   его в строку. Это пример языковой конструкции, известной как *атрибут*,
   обозначенной синтаксисом :ada:`'`, который будет рассмотрен более подробно
   позже.

-  Символ :ada:`&` является оператором конкатенации для строковых значений.

-  The :ada:`end loop` обозначает конец цикла

"Шаг" цикла ограничен 1 (направление вперед) и -1 (назад). Чтобы
выполнить итерацию в обратном направлении по диапазону, используйте
ключевое слово :ada:`reverse`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_5a_Reverse

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet_5a_Reverse is
    begin
       for I in reverse 1 .. 5 loop
          Put_Line ("Hello, World!"
                    & Integer'Image (I));
       end loop;
    end Greet_5a_Reverse;

.. only:: builder_html

    Исполнение этой процедуры дает следующий результат:

    .. code-block:: sh

       Hello, World! 5
       Hello, World! 4
       Hello, World! 3
       Hello, World! 2
       Hello, World! 1

Границы цикла :ada:`for` могут быть вычислены во время выполнения; они
вычисляются один раз, перед выполнением тела цикла. Если значение
верхней границы меньше значения нижней границы, то цикл вообще не
выполняется. Это относится и к обратным циклам. Таким образом, в
следующем примере вывод не производится:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_No_Op

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet_No_Op is
    begin
       for I in reverse 5 .. 1 loop
          Put_Line ("Hello, World!"
                    & Integer'Image (I));
       end loop;
    end Greet_No_Op;

Цикл :ada:`for` является более общим, чем то, что мы проиллюстрировали здесь;
подробнее об этом позже.

Простой цикл
~~~~~~~~~~~~

Самый простой цикл в Ada ‑ это голый цикл, который образует основу
других видов циклов Ada.

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_5b

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet_5b is
       --  Variable declaration:
       I : Integer := 1;
       --  ^ Type
       --             ^ Initial value
    begin
       loop
          Put_Line ("Hello, World!"
                    & Integer'Image (I));

          --  Exit statement:
          exit when I = 5;
          --        ^ Boolean condition

          --  Assignment:
          I := I + 1;
          --  There is no I++ short form to
          --  increment a variable
       end loop;
    end Greet_5b;

Этот пример имеет тот же эффект, что и :ada:`Greet_5a`, показанный ранее.

Это иллюстрирует несколько концепций:

-  Мы объявили переменную с именем :ada:`I` между :ada:`is` и :ada:`begin`.
   Это представляет собой
   *декларативную область*. Ada чётко отделяет декларативную область от
   операторской части подпрограммы. Объявление может отображаться в
   декларативной области, но не допускается в качестве инструкции.

-  Оператор простого цикла начинается с ключевого слова :ada:`loop` и, как и любой
   другой тип оператора цикла, завершается комбинацией ключевых слов :ada:`end loop`.
   Сам по себе это бесконечный цикл. Вы можете выйти из этого цикла с
   помощью оператора выхода :ada:`exit`.

-  Синтаксис для присваивания «:ada:`:=`», а синтаксис для равенства «:ada:`=`». Их
   невозможно перепутать, потому что, как отмечалось ранее, в Ada
   операторы и выражения различны, а выражения не являются допустимыми
   операторами.

Циклы While
~~~~~~~~~~~

Последний вид цикла в Ada ‑ это цикл :ada:`while`.

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_5c

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet_5c is
       I : Integer := 1;
    begin
       --  Condition must be a Boolean value
       --  (no Integers).
       --  Operator "<=" returns a Boolean
       while I <= 5 loop
          Put_Line ("Hello, World!"
                    & Integer'Image (I));

          I := I + 1;
       end loop;
    end Greet_5c;

Условие оценивается перед каждой итерацией. Если результат равен
false, то цикл завершается.

Эта программа имеет тот же эффект, что и предыдущие примеры.

.. admonition:: На других языках

    Обратите внимание, что Ada имеет иную семантику, чем языки на основе
    C, в отношении условия в цикле while. В Ada условие должно быть
    логическим значением, иначе компилятор отклонит программу; условие не
    является целым числом, которое рассматривается как истинное или
    ложное в зависимости от того, является ли оно ненулевым или
    нулевым.

Императивы языка – оператор Case
--------------------------------

Оператор Ada :ada:`case` аналогичен оператору C и C++ :c:`switch`,
но с некоторыми важными отличиями.

Вот пример, вариация программы, которая была показана ранее с
инструкцией :ada:`if`:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Check_Direction_2

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

    procedure Check_Direction is
       N : Integer;
    begin
       loop
          Put ("Enter an integer value: ");
          Get (N);
          Put (N);

          case N is
             when 0 | 360 =>
                Put_Line (" is due north");
             when 1 .. 89 =>
                Put_Line (" is in the northeast quadrant");
             when 90 =>
                Put_Line (" is due east");
             when 91 .. 179 =>
                Put_Line (" is in the southeast quadrant");
             when 180 =>
                Put_Line (" is due south");
             when 181 .. 269 =>
                Put_Line (" is in the southwest quadrant");
             when 270 =>
                Put_Line (" is due west");
             when 271 .. 359 =>
                Put_Line (" is in the northwest quadrant");
             when others =>
                Put_Line (" Au revoir");
                exit;
          end case;
       end loop;
    end Check_Direction;

Эта программа неоднократно запрашивает целочисленное значение, а
затем, если значение находится в диапазоне :ada:`0 .. 360`, отображает
соответствующий квадрант или ось. Если значение является целым числом
за пределами этого диапазона, цикл (и программа) завершаются после
вывода прощального сообщения.

Эффект оператора case аналогичен оператору if в предыдущем примере, но
оператор case может быть более эффективным, поскольку он не включает
несколько тестов диапазона.

Примечательные моменты в операторе Ada case:

-  Выражение case (здесь переменная :ada:`N`) должно быть дискретного типа, то
   есть либо целочисленного типа, либо типа перечисления.
   :ref:`Дискретные типы <WhatIsAType>` будут рассмотрены более подробно позже.

-  Каждое возможное значение для выражения case должно быть охвачено
   уникальной ветвью оператора case. Это будет проверено во время
   компиляции.

-  Ветвь может указывать одно значение, например :ada:`0`; диапазон значений,
   например :ada:`1 .. 89`; или любая их комбинация (разделенные знаком `|`).

-  В качестве особого случая необязательная конечная ветвь может быть
   задана ключевым словом :ada:`others`, которая охватывает все значения, не
   включенные в более ранние ветки.

-  Выполнение состоит из вычисления выражения case, а затем передачи
   управления последовательности инструкций в уникальной ветви, которая
   охватывает это значение.

-  Когда выполнение операторов в выбранной ветви завершено, управление
   возобновляется после последней ветви (после :ada:`end case`).
   В отличие от C, выполнение не
   попадает в следующую ветвь. Таким образом, Ada не нужен (и не имеет)
   оператор :c:`break`.

Императивы языка – декларативные регионы
----------------------------------------

Как упоминалось ранее, Ada проводит четкое синтаксическое разделение
между объявлениями, которые вводят имена для сущностей, которые будут
использоваться в программе, и операторами, выполняющими обработку.
Области в программе, в которых могут появляться объявления, называются
декларативными областями.

В любой подпрограмме раздел между :ada:`is` и :ada:`begin` является
декларативной областью. Там могут быть переменные, константы, типы,
внутренние подпрограммы и другие сущности.

Мы кратко упоминали объявления переменных в предыдущем подразделе.
Давайте посмотрим на простой пример, где мы объявляем целочисленную
переменную :ada:`X` в декларативной области и выполняем инициализацию и
добавление к ней единицы:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Variable_Declaration

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       X : Integer;
    begin
       X := 0;
       Put_Line ("The initial value of X is "
                 & Integer'Image (X));

       Put_Line ("Performing operation on X...");
       X := X + 1;

       Put_Line ("The value of X now is "
                 & Integer'Image (X));
    end Main;

Давайте рассмотрим пример вложенной процедуры:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Nested_Procedure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       procedure Nested is
       begin
          Put_Line ("Hello World");
       end Nested;
    begin
       Nested;
       --  Call to Nested
    end Main;

Объявление не может отображаться как оператор. Если вам нужно объявить
локальную переменную среди операторов, вы можете ввести новую
декларативную область с помощью блочного оператора:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Greet_6

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
    begin
       loop
          Put_Line ("Please enter your name: ");

          declare
             Name : String := Get_Line;
             --               ^ Call to the
             --                 Get_Line function
          begin
             exit when Name = "";
             Put_Line ("Hi " & Name & "!");
          end;

          --  Name is undefined here
       end loop;

      Put_Line ("Bye!");
    end Greet;

.. attention::

    Функция :ada:`Get_Line` позволяет получать входные данные от пользователя и выдавать
    результат в виде строки. Это более или менее эквивалентно функции :c:`scanf` C.

    Функция Get_Line возвращает строку (тип :ada:`String`), которая, как мы увидим позже,
    является :ref:`неограниченным типом массива <UnconstrainedArrayTypes>`.
    Пока мы просто отмечаем, что,
    если вы хотите объявить строковую переменную (с типом :ada:`String`) и заранее не
    знаете ее размер, вам необходимо инициализировать переменную во время ее
    объявления.

Императивы языка – условные выражения
-------------------------------------

В Ada 2012 был введен аналог выражения для условных операторов (:ada:`if` и :ada:`case`).

Выражение If
~~~~~~~~~~~~

Вот альтернативная версия примера приведённого выше; операторы :ada:`if`
заменены на :ada:`if` выражения:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Imperative_Language.Check_Positive

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

    procedure Check_Positive is
       N : Integer;
    begin
       Put ("Enter an integer value: ");
       Get (N);
       Put (N);

       declare
          S : constant String :=
            (if N > 0 then " is a positive number"
             else " is not a positive number");
       begin
          Put_Line (S);
       end;
    end Check_Positive;

Выражение :ada:`if` вычисляет одну из двух строк в зависимости от N и
присваивает это значение локальной переменной S.

Выражения :ada:`if` Ada аналогичны операторам :ada:`if`. Однако есть несколько отличий,
которые проистекают из того факта, что это выражение:

-  Выражения всех ветвей должны быть одного типа

-  Оно *должно* быть заключено в круглые скобки, если окружающее выражение
   их еще не содержит

-  Ветвь :ada:`else` обязательна, если только следующее за :ada:`then` выражение не имеет
   логического значения. В этом случае ветвь :ada:`else` является необязательной и,
   если она отсутствует, по умолчанию имеет значение :ada:`else True`.

Вот еще один пример:

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Even_Odd

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
    begin
       for I in 1 .. 10 loop
          Put_Line (if I mod 2 = 0 then "Even" else "Odd");
       end loop;
    end Main;

Эта программа выдает 10 строк вывода, чередующихся между "Нечетными" и
"Четными" ("Odd" и "Even").

Выражение Case
~~~~~~~~~~~~~~

Аналогично выражениям :ada:`if`, в Ada также есть выражения :ada:`case`. Они работают
именно так, как вы и ожидали.

.. code:: ada run_button project=Courses.Intro_To_Ada.Imperative_Language.Case_Expression

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
    begin
       for I in 1 .. 10 loop
          Put_Line (case I is
                    when 1 | 3 | 5 | 7 | 9 => "Odd",
                    when 2 | 4 | 6 | 8 | 10 => "Even");
       end loop;
    end Main;

Эта программа имеет тот же эффект, что и в предыдущем примере.

Синтаксис отличается от операторов :ada:`case`, ветви разделены запятыми.
