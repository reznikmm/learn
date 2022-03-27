Массивы
=======

.. include:: ../../global.txt

Массивы предоставляют еще одно фундаментальное семейство составных
типов в Ada.

Объявление типа массива
-----------------------

Массивы в Ada используются для определения непрерывных коллекций
элементов, которые можно выбрать путем индексации.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;
       type Index is range 1 .. 5;

       type My_Int_Array is
         array (Index) of My_Int;
       --                 ^ Type of elements
       --       ^ Bounds of the array
       Arr : My_Int_Array := (2, 3, 5, 7, 11);
       --                    ^ Array literal
       --                      (aggregate)

       V : My_Int;
    begin
       for I in Index loop
          V := Arr (I);
          --        ^ Take the Ith element
          Put (My_Int'Image (V));
       end loop;
       New_Line;
    end Greet;

Первое, что следует отметить, - это то, что мы указываем тип индекса
для массива, а не его размер. Здесь мы объявили целочисленный тип с
именем :ada:`Index` в диапазоне от :ada:`1` до :ada:`5`, поэтому каждый
экземпляр массива будет иметь 5 элементов, с начальным элементом с индексом
1 и последним элементом с индексом 5.

Хотя в этом примере для индекса использовался целочисленный тип,
подход Ada является более общим: любой дискретный тип может
индексировать массив, включая типы перечислений
(:ref:`Enum types <EnumTypes>`). Скоро мы увидим, что это значит.

Еще один момент, который следует отметить, заключается в том, что
запрос элемента массива по заданному индексу использует тот же
синтаксис, что и для вызовов функций: то есть объект массива, за
которым следует индекс, который заключен в круглые скобки.

Таким образом, когда вы видите выражение, такое как :ada:`A (B)`, является
ли оно вызовом функции или индексом массива, зависит от того, на что
ссылается :ada:`A`.

Наконец, обратите внимание, как мы инициализируем массив выражением
:ada:`(2, 3, 5, 7, 11)`. Это еще один вид агрегата в Ada, который в
некотором смысле является буквальным выражением для массива, точно так
же, как :ada:`3` является буквальным выражением для целого числа.
Обозначения очень мощные, с рядом свойств, которые мы представим позже.
Подробный обзор появляется в нотации агрегированных типов
(:ref:`aggregate types <Aggregates>`)

Не связанный с массивами, пример также иллюстрирует две процедуры
из:ada:`Ada.Text_IO`:

*  :ada:`Put` ‑ выводит строку без завершающего конца строки.

*  :ada:`New_Line` ‑ вывод конца строки

Давайте теперь углубимся в то, что значит иметь возможность
использовать любой дискретный тип для индексации в массиве.

.. admonition:: На других языках

    Семантически объект массива в Ada - это вся структура данных, а не
    просто дескриптор или указатель. В отличие от C и C ++, не существует
    неявной эквивалентности между массивом и указателем на его начальный
    элемент.


.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Array_Bounds_Example is
       type My_Int is range 0 .. 1000;
       type Index is range 11 .. 15;
       --                  ^ Low bound can be any value
       type My_Int_Array is array (Index) of My_Int;
       Tab : constant My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Index loop
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Array_Bounds_Example;

Одним из эффектов является то, что границы массива могут быть любыми
значениями. В первом примере мы создали тип массива, первый индекс
которого равен :ada:`1`, но в приведенном выше примере мы объявляем тип
массива, первый индекс которого равен :ada:`11`.

Это прекрасно работает в Ada, и, кроме того, поскольку мы используем
тип индекса в качестве диапазона для перебора индексов массива, код,
использующий массив, изменять не нужно.

Это приводит нас к важному следствию, касающемуся кода, работающего с
массивами. Поскольку границы могут варьироваться, вы не должны
предполагать или жестко указывать определенные границы при повторении
или использовании массивов. Это означает, что приведенный выше код
хорош, потому что он использует тип индекса, но цикл for, показанный
ниже, является плохой практикой, даже если он работает правильно:

.. code-block:: ada

    for I in 11 .. 15 loop
       Tab (I) := Tab (I) * 2;
    end loop;

Поскольку для индексации массива можно использовать любой дискретный
тип, разрешены типы перечисления.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Month_Example is
       type Month_Duration is range 1 .. 31;
       type Month is (Jan, Feb, Mar, Apr,
                      May, Jun, Jul, Aug,
                      Sep, Oct, Nov, Dec);

       type My_Int_Array is
         array (Month) of Month_Duration;
       --       ^ Can use an enumeration type
       --         as the index

       Tab : constant My_Int_Array :=
       --    ^ constant is like a variable but
       --      cannot be modified
         (31, 28, 31, 30, 31, 30,
          31, 31, 30, 31, 30, 31);
       --  Maps months to number of days
       --  (ignoring leap years)

       Feb_Days : Month_Duration := Tab (Feb);
       --  Number of days in February
    begin
       for M in Month loop
          Put_Line
            (Month'Image (M) & " has "
             & Month_Duration'Image (Tab (M))
             & " days.");
       --    ^ Concatenation operator
       end loop;
    end Month_Example;

В приведенном выше примере мы:

-  Создание типа массива, отображающего продолжительность месяцев в днях.

-  Создание массива и создание его экземпляра с агрегированным
   сопоставлением месяцев с их фактической продолжительностью в днях.

-  Итерация по массиву, печать месяцев и количество дней для каждого.

Возможность использования значений перечисления в качестве индексов
очень полезна при создании сопоставлений, как показано выше, и часто
используется в Ada.

Индексирование
--------------

Мы уже видели синтаксис для выбора элементов массива. Однако следует
отметить еще несколько моментов.

Во-первых, как и в целом в Ada, операция индексирования строго
типизирована. Если для индексации массива используется значение
неправильного типа, будет получена ошибка времени компиляции.

.. code-block:: ada
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;

       type My_Index   is range 1 .. 5;
       type Your_Index is range 1 .. 5;

       type My_Int_Array is array (My_Index) of My_Int;
       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Your_Index loop
          Put (My_Int'Image (Tab (I)));
       --                         ^ Compile time error
       end loop;
       New_Line;
    end Greet;

Во-вторых, массивы в Ada проверяются на границы. Это означает, что
если вы попытаетесь получить доступ к элементу за пределами массива,
вы получите ошибку во время выполнения вместо доступа к случайной
памяти, как в небезопасных языках.

.. code-block:: ada
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;
       type Index is range 1 .. 5;
       type My_Int_Array is array (Index) of My_Int;
       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Index range 2 .. 6 loop
          Put (My_Int'Image (Tab (I)));
          --                      ^ Will raise an
          --                        exception when
          --                        I = 6
       end loop;
       New_Line;
    end Greet;

Более простые объявления массива
--------------------------------

В предыдущих примерах мы всегда явно создавали тип индекса для
массива. Хотя это может быть полезно для типизации и удобства чтения,
иногда вам просто нужно выразить диапазон значений. Ada тоже позволяет
вам это делать.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Simple_Array_Bounds is
       type My_Int is range 0 .. 1000;
       type My_Int_Array is array (1 .. 5) of My_Int;
       --                          ^ Subtype of Integer
       Tab : constant My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in 1 .. 5 loop
       --       ^ Subtype of Integer
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Simple_Array_Bounds;

В этом примере диапазон массива определяется с помощью синтаксиса
диапазона, который указывает анонимный подтип Integer и использует его
для индексации массива.

Это означает, что индекс имеет целочисленный тип :ada:`Integer`. Точно так
же, когда вы используете анонимный диапазон в цикле for, как в приведенном
выше примере, тип переменной итерации также является :ada:`Integer`, поэтому
вы можете использовать :ada:`I` для индексации :ada:`Tab`.

Вы также можете использовать именованный подтип для границ массива.

Атрибут диапазона
-----------------

Ранее мы отметили, что жесткие границы кодирования при итерации по
массиву ‑ плохая идея, и показали, как использовать тип / подтип
индекса массива для итерации по его диапазону в цикле for. Это
поднимает вопрос о том, как написать итерацию, когда массив имеет
анонимный диапазон для своих границ, поскольку нет имени для ссылки на
диапазон. Ada решает эту проблему с помощью нескольких атрибутов
объектов массива:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Range_Example is
       type My_Int is range 0 .. 1000;
       type My_Int_Array is array (1 .. 5) of My_Int;
       Tab : constant My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Tab'Range loop
       --          ^ Gets the range of Tab
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Range_Example;

Если требуется более детализированное управление, можно использовать
отдельные атрибуты «Первый» (:ada:`'First`) и «Последний» (:ada:`'Last`).

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Array_Attributes_Example is
       type My_Int is range 0 .. 1000;
       type My_Int_Array is array (1 .. 5) of My_Int;
       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Tab'First .. Tab'Last - 1 loop
       --          ^ Iterate on every index
       --            except the last
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Array_Attributes_Example;

Атрибуты :ada:`'Range`, :ada:`'First` и :ada:`'Last` в этих примерах также
могли применяться к имени типа массива, а не только к экземплярам массива.

Хотя это не показано в вышеприведенных примерах, другим полезным
атрибутом для экземпляра массива :ada:`A` является :ada:`A'Length`, который
является количеством элементов, содержащихся в :ada:`A`.

Законно и иногда полезно иметь «пустой массив» (null array), который
не содержит элементов. Чтобы получить этот эффект, определите диапазон
индексов, верхняя граница которого меньше нижней границы.

.. _UnconstrainedArrayTypes:

Неограниченные массивы
----------------------

Давайте теперь рассмотрим один из самых мощных аспектов средства
массивов Ada.

Каждый тип массива, который мы определили до сих пор, имеет
фиксированный размер: каждый экземпляр этого типа будет иметь
одинаковые границы и, следовательно, одинаковое количество элементов и
одинаковый размер.

Однако Ada также позволяет объявлять типы массивов, границы которых не
являются фиксированными: в этом случае границы необходимо будет
указать при создании экземпляров типа.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Unconstrained_Array_Example is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       type Workload_Type is
         array (Days range <>) of Natural;
       --  Indefinite array type
       --       ^ Bounds are of type Days,
       --         but not known

       Workload : constant
         Workload_Type (Monday .. Friday) :=
       --               ^ Specify the bounds
       --                 when declaring
          (Friday => 7, others => 8);
       --               ^ Default value
       --  ^ Specify element by name of index
    begin
       for I in Workload'Range loop
          Put_Line (Integer'Image (Workload (I)));
       end loop;
    end Unconstrained_Array_Example;

Тот факт, что границы массива неизвестны, указывается синтаксисом 
:ada:`Days range <>`. Учитывая дискретный тип :ada:`Discrete_Type`,
если мы используем :ada:`Discrete_Type` для индекса в типе массива,
тогда :ada:`Discrete_Type` служит типом индекса и включает диапазон
значений индекса для каждого экземпляра массива.

Если мы определяем индекс как :ada:`Discrete_Type range <>`,
тогда :ada:`Discrete_Type` служит типом индекса, но разные
экземпляры массива могут иметь границы, отличные от этого типа.

Тип массива, который определяется с помощью синтаксиса
:ada:`Discrete_Type range <>` для его
индекса, называется неограниченным типом массива, и, как показано
выше, при создании экземпляра необходимо указать границы.

В приведенном выше примере также показаны другие формы синтаксиса
агрегата. Вы можете указать ассоциации по имени, задав значение
индекса слева от ассоциации со стрелкой. :ada:`1 => 2`, таким образом,
означает «присвоить значение 2 элементу с индексом 1 в моем массиве».
:ada:`others => 8` означает «присвоить значение 8 каждому элементу,
который ранее не был назначен в этом агрегате».

.. attention::
    Так называемая «коробочная» нотация (:ada:`<>`) обычно
    используется в качестве подстановочного знака или местозаполнителя в
    Ada. Вы часто увидите это, когда смысл «то, что здесь ожидается, может
    быть чем угодно».

.. admonition:: На других языках

    В то время как неограниченные массивы в Ada могут казаться похожими на
    массивы переменной длины в C, в действительности они гораздо более
    мощные, потому что они действительно первоклассные значения в языке.
    Их можно передавать в качестве параметров подпрограммам или возвращать
    из функций, и они неявно содержат их границы как часть их значения.
    Это означает, что бесполезно явно передавать границы или длину массива
    вместе с массивом, поскольку они доступны через атрибуты :ada:`'First`,
    :ada:`'Last`, :ada:`'Range` и :ada:`'Length`, описанные ранее.

Хотя различные экземпляры одного и того же неограниченного типа
массива могут иметь разные границы, конкретный экземпляр имеет
одинаковые границы в течение всего срока его существования. Это
позволяет Ada эффективно реализовывать неограниченные массивы;
экземпляры могут храниться в стеке и не требуют выделения кучи, как в
таких языках, как Java.

Предопределенный тип массива: String
------------------------------------

Повторяющейся темой в нашем введении к типам Ada было то, как важные
встроенные типы, такие как :ada:`Boolean` или :ada:`Integer`, определяются
через те же средства, которые доступны пользователю. Это также верно для
строк: тип :ada:`String` в Ada является простым массивом.

Вот как тип строки определяется в Ada:

.. code-block:: ada

    type String is array (Positive range <>) of Character;

Единственной встроенной функцией, которую Ada добавляет, чтобы сделать
строки более эргономичными, являются пользовательские литералы, как мы
можем видеть в примере ниже.

.. hint::
    Строковые литералы являются синтаксическим сахаром для агрегатов, так
    что в следующем примере :ada:`A` и :ada:`B` имеют одинаковое значение.

    .. code-block:: ada

        package String_Literals is
            --  Those two declarations are equivalent
            A : String (1 .. 11) := "Hello World";
            B : String (1 .. 11) :=
                ('H', 'e', 'l', 'l', 'o', ' ',
                 'W', 'o', 'r', 'l', 'd');
        end String_Literals;

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       Message : String (1 .. 11) := "dlroW olleH";
       --        ^ Pre-defined array type.
       --          Component type is Character
    begin
       for I in reverse Message'Range loop
          --    ^ Iterate in reverse order
          Put (Message (I));
       end loop;
       New_Line;
    end Greet;

Однако явное указание границ объекта ‑ это немного хлопотно; вам нужно
вручную подсчитать количество символов в литерале. К счастью, Ada
предлагает более простой способ.

Вы можете опустить границы при создании экземпляра неограниченного
типа массива, если вы предоставляете инициализацию, поскольку границы
могут быть выведены из выражения инициализации.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       Message : constant String := "dlroW olleH";
       --                 ^ Bounds are automatically
       --                   computed from
       --                   initialization value
    begin
       for I in reverse Message'Range loop
          Put (Message (I));
       end loop;
       New_Line;
    end Greet;

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Integer_Array is array (Natural range <>) of Integer;

       My_Array : constant Integer_Array := (1, 2, 3, 4);
       --                  ^ Bounds are automatically
       --                    computed from
       --                    initialization value
    begin
        null;
    end Main;

.. attention::
    Как вы можете видеть выше, стандартный тип `String` в Ada ‑ это массив.
    Таким образом, он разделяет преимущества и недостатки массивов: значение
    :ada:`String` выделяется в стеке, доступ к нему осуществляется эффективно,
    а его границы неизменны.

    Если вам нужно что-то вроде :c++:`std::string` в C ++, вы можете
    использовать :ref:`Unbounded Strings <UnboundedStrings>` из
    стандартной библиотеки Ada. Этот тип больше похож на изменяемый,
    автоматически управляемый строковый буфер, в который вы можете
    добавлять содержимое.

Ограничения
-----------

Очень важный момент, касающийся массивов: границы *должны* быть
известны при создании экземпляров. Например, незаконно делать
следующее.

.. code-block:: ada

    declare
       A : String;
    begin
       A := "World";
    end;

Кроме того, хотя вы, конечно, можете изменять значения элементов в
массиве, вы не можете изменять границы массива (и, следовательно, его
размер) после его инициализации. Так что это тоже незаконно:

.. code-block:: ada

    declare
       A : String := "Hello";
    begin
       A := "World";       --  OK: Same size
       A := "Hello World"; --  Not OK: Different size
    end;

Кроме того, хотя вы можете ожидать предупреждения об ошибке такого
рода в очень простых случаях, подобных этому, компилятор в общем
случае не может знать, присваиваете ли вы значение правильной длины,
поэтому это нарушение обычно приведет к ошибке во время выполнения.

.. admonition:: Обратите внимание

    Хотя мы узнаем об этом позже, важно знать, что массивы - не
    единственные типы, экземпляры которых могут быть неизвестного размера
    во время компиляции.

    Говорят, что такие объекты относятся к *неопределенному подтипу*, что
    означает, что размер подтипа неизвестен во время компиляции, но
    динамически вычисляется (во время выполнения).

    .. code-block:: ada

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Indefinite_Subtypes is
            function Get_Number return Integer is
            begin
                return Integer'Value (Get_Line);
            end Get_Number;

           A : String := "Hello";
           --  Indefinite subtype

           B : String (1 .. 5) := "Hello";
           --  Definite subtype

           C : String (1 .. Get_Number);
           --  Indefinite subtype
           --  (Get_Number's value is computed at
           --  run-time)
        begin
           null;
        end Indefinite_Subtypes;

Возврат неограниченных массивов
-------------------------------

Тип возвращаемого значения функции может быть любым; функция может
возвращать значение, размер которого неизвестен во время компиляции.
Точно так же параметры могут быть любого типа.

Например, это функция, которая возвращает неограниченную строку:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is

       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       function Get_Day_Name (Day : Days := Monday)
                              return String is
       begin
          return
            (case Day is
             when Monday    => "Monday",
             when Tuesday   => "Tuesday",
             when Wednesday => "Wednesday",
             when Thursday  => "Thursday",
             when Friday    => "Friday",
             when Saturday  => "Saturday",
             when Sunday    => "Sunday");
       end Get_Day_Name;

    begin
       Put_Line ("First day is "
                 & Get_Day_Name (Days'First));
    end Main;

Примечание. Этот пример приведен только в иллюстративных целях.
Существует встроенный механизм, атрибут :ada:`'Image` для скалярных типов,
который возвращает имя (в виде строки – типа :ada:`String`) любого элемента
типа перечисления. Например, :ada:`Days'Image(Monday)` есть :ada:`"MONDAY"`.)

.. admonition:: На других языках

    Возврат объектов переменного размера в языках, в которых отсутствует
    сборщик мусора, немного сложен с точки зрения реализации, поэтому C и
    C++ не допускают этого, предпочитая зависеть от явного динамического
    выделения/свободного от пользователя.

    Проблема в том, что явное управление хранилищем становится
    небезопасным, как только вы захотите собрать неиспользуемую память.
    Способность Ada возвращать объекты переменного размера удалит один
    вариант использования для динамического распределения и,
    следовательно, удалит один потенциальный источник ошибок из ваших
    программ.

    Rust следует модели C/C++, но с безопасной семантикой указателя.
    Однако динамическое распределение все еще используется. Ada может
    извлечь выгоду из возможного повышения производительности, поскольку
    она может использовать любую модель.

Объявление массивов (2)
-----------------------

Хотя мы можем иметь типы массивов, размер и границы которых
определяются во время выполнения, тип компонента массива должен быть
определенного и ограниченного типа.

Таким образом, если необходимо объявить, например, массив строк,
подтип :ada:`String`, используемый в качестве компонента, должен иметь
фиксированный размер.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Days is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       subtype Day_Name is String (1 .. 2);
       --  Subtype of string with known size

       type Days_Name_Type is
         array (Days) of Day_Name;
       --       ^ Type of the index
       --                ^ Type of the element.
       --                  Must be definite

       Names : constant Days_Name_Type :=
         ("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su");
       --  Initial value given by aggregate
    begin
       for I in Names'Range loop
          Put_Line (Names (I));
       end loop;
    end Show_Days;

Срезы массива
-------------

Последняя особенность массивов Ada, которую мы собираемся рассмотреть,
‑ это фрагменты массива. Можно взять и использовать фрагмент массива
(непрерывную последовательность элементов) в качестве имени или
значения.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
        Buf : String := "Hello ...";

        Full_Name : String := "John Smith";
    begin
        Buf (7 .. 9) := "Bob";
        --  Careful! This works because the string
        --  on the right side is the same length as
        --  the replaced slice!

        --  Prints "Hello Bob"
        Put_Line (Buf);

         --  Prints "Hi John"
        Put_Line ("Hi " & Full_Name (1 .. 4));
    end Main;

Как мы видим выше, вы можете использовать срез слева от присваивания,
чтобы заменить только часть массива.

Срез массива имеет тот же тип, что и массив, но имеет другой подтип,
ограниченный границами среза.

.. attention::
    В Ada есть `многомерные массивы
    <http://www.adaic.org/resources/add_content/standards/12rm/html/RM-3-6.html>`_,
    которые не рассматриваются в этом курсе. Срезы будут работать только
    с одномерными массивами.

.. _Object_Renaming:

Переименование
--------------

До сих пор мы видели, что следующие элементы могут быть переименованы:
:ref:`подпрограммы <Subprogram_Renaming>`, :ref:`пакеты <Package_Renaming>`
и :ref:`компоненты записи <Record_Comp_Renaming>`. Мы также можем
переименовывать объекты с помощью ключевого слова :ada:`renames`. Это
позволяет создавать альтернативные имена для этих объектов. Давайте
посмотрим на пример:

.. code-block:: ada

    package Measurements is

       subtype Degree_Celsius is Float;

       Current_Temperature : Degree_Celsius;

    end Measurements;

    with Ada.Text_IO;  use Ada.Text_IO;
    with Measurements;

    procedure Main is
       subtype Degrees is Measurements.Degree_Celsius;

       T : Degrees
         renames Measurements.Current_Temperature;
    begin
        T := 5.0;

        Put_Line (Degrees'Image (T));
        Put_Line (Degrees'Image
          (Measurements.Current_Temperature));

        T := T + 2.5;

        Put_Line (Degrees'Image (T));
        Put_Line (Degrees'Image
          (Measurements.Current_Temperature));
    end Main;

В приведенном выше примере мы объявляем переменную :ada:`T`, переименовывая
объект :ada:`Current_Temperature` из пакета :ada:`Measurements`. Как вы
можете видеть, запустив этот пример, и :ada:`Current_Temperature`, и
его альтернативное имя :ada:`T` имеют одинаковые значения:

-  сначала они показывают значение 5.0

-  после сложения они показывают значение 7,5.

Это потому, что они по существу относятся к одному и тому же объекту,
но с двумя разными именами.

Обратите внимание, что в приведенном выше примере мы используем :ada:`Degrees`
как псевдоним :ada:`Degree_Celsius`. Мы обсуждали этот метод
:ref:`ранее в курсе <SubtypeAliases>`.

Переименование может быть полезно для улучшения читаемости более
сложной индексации массивов. Вместо того, чтобы явно использовать
индексы каждый раз, когда мы обращаемся к определенным позициям
массива, мы можем создавать более короткие имена для этих позиций,
переименовывая их. Давайте посмотрим на следующий пример:

.. code-block:: ada

    package Colors is

       type Color is (Black, Red, Green, Blue, White);

       type Color_Array is
         array (Positive range <>) of Color;

       procedure Reverse_It (X : in out Color_Array);

    end Colors;

    package body Colors is

       procedure Reverse_It (X : in out Color_Array) is
       begin
          for I in X'First .. (X'Last + X'First) / 2 loop
             declare
                Tmp     : Color;
                X_Left  : Color
                  renames X (I);
                X_Right : Color
                  renames X (X'Last + X'First - I);
             begin
                Tmp     := X_Left;
                X_Left  := X_Right;
                X_Right := Tmp;
             end;
          end loop;
       end Reverse_It;

    end Colors;

    with Ada.Text_IO; use Ada.Text_IO;

    with Colors; use Colors;

    procedure Test_Reverse_Colors is

       My_Colors : Color_Array (1 .. 5) :=
         (Black, Red, Green, Blue, White);

    begin
       for C of My_Colors loop
          Put_Line ("My_Color: " & Color'Image (C));
       end loop;

       New_Line;
       Put_Line ("Reversing My_Color...");
       New_Line;
       Reverse_It (My_Colors);

       for C of My_Colors loop
          Put_Line ("My_Color: " & Color'Image (C));
       end loop;

    end Test_Reverse_Colors;

В приведенном выше примере пакет :ada:`Colors` реализует процедуру
:ada:`Reverse_It`, объявляя новые имена для двух позиций массива.
Фактическая реализация становится легко читаемой:

.. code-block:: ada

    begin
       Tmp     := X_Left;
       X_Left  := X_Right;
       X_Right := Tmp;
    end;

Сравните это с альтернативной версией без переименования:

.. code-block:: ada

    begin
       Tmp                      := X (I);
       X (I)                    := X (X'Last + X'First - I);
       X (X'Last + X'First - I) := Tmp;
    end;
