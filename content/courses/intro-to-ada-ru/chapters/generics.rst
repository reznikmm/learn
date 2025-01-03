Настраиваемые модули
====================

.. include:: ../../global.txt

Введение
--------

Настраиваемые модули в Аде используются для метапрограммирования. Когда некоторые
алгоритмы имеют достаточно много общего и отличаются лишь деталями, можно выделить
абстрактный алгоритм воспользовавшись возможностями настраиваемых модулей.

Настраиваемыми могут быть лишь подпрограммы или пакеты. Объявление настраиваемого
модуля начинается с ключевого слова :ada:`generic`. Например:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Generics.Show_Simple_Generic

    generic
       type T is private;
       --  Declaration of formal types and objects
    --  Below, we could use one of the following:
    --  <procedure | function | package>
    procedure Operator (Dummy : in out T);

    procedure Operator (Dummy : in out T) is
    begin
       null;
    end Operator;

Объявление формального типа
---------------------------

Формальные типы - это абстракции типа некоторого класса. Например, мы
можем понадобиться создать алгоритм, который работает с любым
целочисленным типом или даже с любым типом вообще, будь то числовой
тип или нет. В следующем примере объявляется формальный тип :ada:`T` для
процедуры :ada:`Set`.

.. code:: ada compile_button project=Courses.Intro_To_Ada.Generics.Show_Formal_Type_Declaration

    generic
       type T is private;
       --  T is a formal type that indicates that
       --  any type can be used, possibly a numeric
       --  type or possibly even a record type.
    procedure Set (Dummy : T);

    procedure Set (Dummy : T) is
    begin
       null;
    end Set;

Объявление :ada:`T` как :ada:`private` указывает на то, что на его месте
может быть любой определенный тип. Но также можно сузить условие, разрешив
подстановку типов лишь некоторого класса. Вот несколько примеров:

+-------------------------------+---------------------------------------+
| Формальный тип                | Формат                                |
+===============================+=======================================+
| Любой тип                     | :ada:`type T is private;`             |
+-------------------------------+---------------------------------------+
| Любой дискретный тип          | :ada:`type T is (<>);`                |
+-------------------------------+---------------------------------------+
| Любой тип с плавающей запятой | :ada:`type T is digits <>;`           |
+-------------------------------+---------------------------------------+

Объявление формального объекта
------------------------------

Формальные объекты аналогичны параметрам подпрограммы. Они могут
ссылаться на формальные типы, объявленные в формальной спецификации.
Например:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Generics.Show_Formal_Object_Declaration

    generic
       type T is private;
       X : in out T;
       --  X can be used in the Set procedure
    procedure Set (E : T);

    procedure Set (E : T) is
       pragma Unreferenced (E, X);
    begin
       null;
    end Set;

Формальные объекты могут быть либо входными параметрами, либо
иметь вид :ada:`in out`.

Определение тела настраиваемого модуля
--------------------------------------

Не нужно повторять ключевое слово :ada:`generic` при объявления тела
настраиваемой подпрограммы или пакета. Для реализации мы используем
синтаксис, как у обычного тела модуля, и используем объявленные выше
формальные типы и объекты.
Например:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Generics.Show_Generic_Body_Definition

    generic
       type T is private;
       X : in out T;
    procedure Set (E : T);

    procedure Set (E : T) is
    --  Body definition: "generic" keyword
    --  is not used
    begin
       X := E;
    end Set;

Конкретизация настройки
-----------------------

Настраиваемую подпрограммы или пакеты нельзя использовать напрямую. Сначала
они должны быть конкретезированы, что мы делаем с помощью ключевого слова
:ada:`new`, как показано в следующем примере:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Show_Generic_Instantiation

    generic
       type T is private;
       X : in out T;
       --  X can be used in the Set procedure
    procedure Set (E : T);

    procedure Set (E : T) is
    begin
       X := E;
    end Set;

    with Ada.Text_IO; use Ada.Text_IO;
    with Set;

    procedure Show_Generic_Instantiation is

       Main    : Integer := 0;
       Current : Integer;

       procedure Set_Main is new Set (T => Integer,
                                      X => Main);
       --  Here, we map the formal parameters to
       --  actual types and objects.
       --
       --  The same approach can be used to
       --  instantiate functions or packages, e.g.:
       --
       --  function Get_Main is new ...
       --  package Integer_Queue is new ...

    begin
       Current := 10;

       Set_Main (Current);
       Put_Line ("Value of Main is "
                 & Integer'Image (Main));
    end Show_Generic_Instantiation;

В приведенном выше примере мы создаем экземпляр настраиваемой процедуры
:ada:`Set`, сопоставляя формальные параметры :ada:`T` и :ada:`X` с
фактическими, уже существующими, элементами, в данном случае типом
:ada:`Integer` и переменной :ada:`Main`.

Настраиваемые пакеты
--------------------

Предыдущие примеры мы сосредочились на настраиваемых подпрограммах. В этом
разделе мы рассмотрим настраиваемые пакеты. Их синтаксис аналогичен: мы
начинаем с ключевого слова :ada:`generic`, а далее следуют формальныме
объявления. Единственное отличие состоит в том, что вместо ключевого слова
подпрограммы указывается :ada:`package`.

Вот пример:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Show_Generic_Package

    generic
       type T is private;
    package Element is

       procedure Set (E : T);
       procedure Reset;
       function Get return T;
       function Is_Valid return Boolean;

       Invalid_Element : exception;

    private
       Value : T;
       Valid : Boolean := False;
    end Element;

    package body Element is

       procedure Set (E : T) is
       begin
          Value := E;
          Valid := True;
       end Set;

       procedure Reset is
       begin
          Valid := False;
       end Reset;

       function Get return T is
       begin
          if not Valid then
             raise Invalid_Element;
          end if;
          return Value;
       end Get;

       function Is_Valid return Boolean is (Valid);
    end Element;

    with Ada.Text_IO; use Ada.Text_IO;
    with Element;

    procedure Show_Generic_Package is

       package I is new Element (T => Integer);

       procedure Display_Initialized is
       begin
          if I.Is_Valid then
             Put_Line ("Value is initialized");
          else
             Put_Line ("Value is not initialized");
          end if;
       end Display_Initialized;

    begin
       Display_Initialized;

       Put_Line ("Initializing...");
       I.Set (5);
       Display_Initialized;
       Put_Line ("Value is now set to "
                 & Integer'Image (I.Get));

       Put_Line ("Reseting...");
       I.Reset;
       Display_Initialized;

    end Show_Generic_Package;

В приведенном выше примере мы создали простой контейнер с именем
:ada:`Element`, содержащий всего один элемент. Этот контейнер
отслеживает, был ли элемент инициализирован или нет.

После написания определения пакета мы создаем экземпляр :ada:`I`
пакета :ada:`Element`. Мы используем экземпляр, вызывая подпрограммы
пакета (:ada:`Set`, :ada:`Reset` и :ada:`Get`).

Формальные подпрограммы
-----------------------

В дополнение к формальным типам и объектам мы также можем объявлять
формальные подпрограммы или пакеты. Этот курс описывает только
формальные подпрограммы; формальные пакеты обсуждаются в продвинутом
курсе.

Мы используем ключевое слово :ada:`with` для объявления формальной
подпрограммы. В приведенном ниже примере мы объявляем формальную
функцию (:ada:`Comparison`), которая будет использоваться настраиваемой
процедурой :ada:`Check`.

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Show_Formal_Subprogram

    generic
       Description : String;
       type T is private;
       with function Comparison (X, Y : T) return Boolean;
    procedure Check (X, Y : T);

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Check (X, Y : T) is
       Result : Boolean;
    begin
       Result := Comparison (X, Y);
       if Result then
          Put_Line ("Comparison ("
                    & Description
                    & ") between arguments is OK!");
       else
          Put_Line ("Comparison ("
                    & Description
                    & ") between arguments is not OK!");
       end if;
    end Check;

    with Check;

    procedure Show_Formal_Subprogram is

       A, B : Integer;

       procedure Check_Is_Equal is new
         Check (Description => "equality",
                T           => Integer,
                Comparison  => Standard."=");
       --  Here, we are mapping the standard
       --  equality operator for Integer types to
       --  the Comparison formal function
    begin
       A := 0;
       B := 1;
       Check_Is_Equal (A, B);
    end Show_Formal_Subprogram;

Пример: конкретизация ввода/вывода
----------------------------------

Ада предлагает настраиваемые пакеты ввода-вывода, которые могут быть
конкретизированы для стандартных и произвольных типов. Одним из примеров
является настраиваемый пакет :ada:`Float_IO`, который предоставляет такие
процедуры, как :ada:`Put` и :ada:`Get`. Фактически, :ada:`Float_Text_IO`
- доступный в стандартной библиотеке - является конкретизацией
пакета :ada:`Float_IO` и определяется как:

.. code-block:: ada

    with Ada.Text_IO;

    package Ada.Float_Text_IO is new Ada.Text_IO.Float_IO (Float);

Его можно использовать непосредственно с любым объектом типа :ada:`Float`.
Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Show_Float_Text_IO

    with Ada.Float_Text_IO;

    procedure Show_Float_Text_IO is
       X : constant Float := 2.5;

       use Ada.Float_Text_IO;
    begin
       Put (X);
    end Show_Float_Text_IO;

Создание экземпляров настраиваемых пакетов ввода-вывода может быть
полезно для пользовательских типов. Например, давайте создадим новый тип :ada:`Price`,
который должен отображаться с двумя десятичными цифрами после точки и
без экспоненты.

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Show_Float_IO_Inst

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Float_IO_Inst is

       type Price is digits 3;

       package Price_IO is new
         Ada.Text_IO.Float_IO (Price);

       P : Price;
    begin
       --  Set to zero => don't display exponent
       Price_IO.Default_Exp  := 0;

       P := 2.5;
       Price_IO.Put (P);
       New_Line;

       P := 5.75;
       Price_IO.Put (P);
       New_Line;
    end Show_Float_IO_Inst;

Регулируя значение :ada:`Default_Exp` экземпляра :ada:`Price_IO` для *удаления*
экспоненты, мы можем контролировать, как отображаются переменные типа
:ada:`Price`. В качестве примечания мы также могли бы написать:

.. code-block:: ada

    -- [...]

       type Price is new Float;

       package Price_IO is new
         Ada.Text_IO.Float_IO (Price);

    begin
       Price_IO.Default_Aft  := 2;
       Price_IO.Default_Exp  := 0;

В этом случае мы также изменяем :ada:`Default_Aft` чтобы при вызове :ada:`Put`
получить две десятичные цифры после запятой.

В дополнение к настраиваемому пакету :ada:`Float_IO` в :ada:`Ada.Text_IO`
доступны следующие настраиваемые пакеты:

-  :ada:`Enumeration_IO` для перечислимых типов;
-  :ada:`Integer_IO` для целочисленных типов;
-  :ada:`Modular_IO` для модульных типов;
-  :ada:`Fixed_IO` для типов с фиксированной запятой;
-  :ada:`Decimal_IO` для десятичных типов.

Фактически, мы могли бы переписать пример выше, используя десятичные
типы:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Show_Decimal_IO_Inst

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Decimal_IO_Inst is

       type Price is delta 10.0 ** (-2) digits 12;

       package Price_IO is new
         Ada.Text_IO.Decimal_IO (Price);

       P : Price;
    begin
       Price_IO.Default_Exp  := 0;

       P := 2.5;
       Price_IO.Put (P);
       New_Line;

       P := 5.75;
       Price_IO.Put (P);
       New_Line;
    end Show_Decimal_IO_Inst;

Пример: АТД
-----------

Важным применением настраиваемых модулей является моделирование
абстрактных типов данных (АТД). Фактически Ада предоставляет библиотеку с
многочисленными АТД, использующими настраиваемые модули: :ada:`Ada.Containers`
(описаны в :ref:`разделе контейнеров <Intro_Ada_Ru_Containers>`).

Типичным примером АТД является стек:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Show_Stack

    generic
       Max : Positive;
       type T is private;
    package Stacks is

       type Stack is limited private;

       Stack_Underflow, Stack_Overflow : exception;

       function Is_Empty (S : Stack) return Boolean;

       function Pop (S : in out Stack) return T;

       procedure Push (S : in out Stack;
                       V :        T);

    private

       type Stack_Array is
         array (Natural range <>) of T;

       Min : constant := 1;

       type Stack is record
          Container : Stack_Array (Min .. Max);
          Top       : Natural := Min - 1;
       end record;

    end Stacks;

    package body Stacks is

       function Is_Empty (S : Stack) return Boolean is
         (S.Top < S.Container'First);

       function Is_Full (S : Stack) return Boolean is
         (S.Top >= S.Container'Last);

       function Pop (S : in out Stack) return T is
       begin
          if Is_Empty (S) then
             raise Stack_Underflow;
          else
             return X : T do
                X     := S.Container (S.Top);
                S.Top := S.Top - 1;
             end return;
          end if;
       end Pop;

       procedure Push (S : in out Stack;
                       V :        T) is
       begin
          if Is_Full (S) then
             raise Stack_Overflow;
          else
             S.Top               := S.Top + 1;
             S.Container (S.Top) := V;
          end if;
       end Push;

    end Stacks;

    with Ada.Text_IO; use Ada.Text_IO;
    with Stacks;

    procedure Show_Stack is

       package Integer_Stacks is new
         Stacks (Max => 10,
                 T   => Integer);
       use Integer_Stacks;

       Values : Integer_Stacks.Stack;

    begin
       Push (Values, 10);
       Push (Values, 20);

       Put_Line ("Last value was "
                 & Integer'Image (Pop (Values)));
    end Show_Stack;

В этом примере сначала создается настраиваемый пакет стека (:ada:`Stacks`), а
затем он конкретизируется чтобы создаеть стек содержащи до 10 целых значений.

Пример: Обмен
-------------

Давайте рассмотрим простую процедуру, которая меняет местами
переменные типа :ada:`Color`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Test_Non_Generic_Swap_Colors

    package Colors is
       type Color is (Black, Red, Green,
                      Blue, White);

       procedure Swap_Colors (X, Y : in out Color);
    end Colors;

    package body Colors is

       procedure Swap_Colors (X, Y : in out Color) is
          Tmp : constant Color := X;
       begin
          X := Y;
          Y := Tmp;
       end Swap_Colors;

    end Colors;

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Test_Non_Generic_Swap_Colors is
       A, B, C : Color;
    begin
       A := Blue;
       B := White;
       C := Red;

       Put_Line ("Value of A is "
                 & Color'Image (A));
       Put_Line ("Value of B is "
                 & Color'Image (B));
       Put_Line ("Value of C is "
                 & Color'Image (C));

       New_Line;
       Put_Line ("Swapping A and C...");
       New_Line;
       Swap_Colors (A, C);

       Put_Line ("Value of A is "
                 & Color'Image (A));
       Put_Line ("Value of B is "
                 & Color'Image (B));
       Put_Line ("Value of C is "
                 & Color'Image (C));
    end Test_Non_Generic_Swap_Colors;

В этом примере :ada:`Swap_Colors` можно использовать только для типа
:ada:`Color`. Однако этот алгоритм теоретически можно использовать
для любого типа, будь то перечислимый тип или составной тип записи с
множеством элементов. Сам алгоритм такой же: отличается только тип.
Если, например, мы хотим поменять местами переменные типа :ada:`Integer`,
мы не хотим дублировать реализацию.
Следовательно, такой алгоритм - идеальный кандидат для абстракции с
использованием настраиваемых модулей.

В приведенном ниже примере мы создадим настраиваемую версию :ada:`Swap_Colors`
и назовем ее :ada:`Generic_Swap`.
Эта настраиваемая версия может работать с любым типом благодаря
объявлению формального типа :ada:`T`.

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Test_Swap_Colors

    generic
       type T is private;
    procedure Generic_Swap (X, Y : in out T);

    procedure Generic_Swap (X, Y : in out T) is
       Tmp : constant T := X;
    begin
       X := Y;
       Y := Tmp;
    end Generic_Swap;

    with Generic_Swap;

    package Colors is

       type Color is (Black, Red, Green,
                      Blue, White);

       procedure Swap_Colors is new
         Generic_Swap (T => Color);

    end Colors;

    with Ada.Text_IO;  use Ada.Text_IO;
    with Colors;       use Colors;

    procedure Test_Swap_Colors is
       A, B, C : Color;
    begin
       A := Blue;
       B := White;
       C := Red;

       Put_Line ("Value of A is "
                 & Color'Image (A));
       Put_Line ("Value of B is "
                 & Color'Image (B));
       Put_Line ("Value of C is "
                 & Color'Image (C));

       New_Line;
       Put_Line ("Swapping A and C...");
       New_Line;
       Swap_Colors (A, C);

       Put_Line ("Value of A is "
                 & Color'Image (A));
       Put_Line ("Value of B is "
                 & Color'Image (B));
       Put_Line ("Value of C is "
                 & Color'Image (C));
    end Test_Swap_Colors;

Как мы видим в примере, мы можем создать ту же процедуру :ada:`Swap_Colors`,
что и в первой версии алгоритма, объявив ее как конкретизацию
настраиваемой процедуры :ada:`Generic_Swap`. Мы сопоставляем формальный тип
:ada:`T` с типом :ada:`Color`, указывая его в качестве
аргумента конкретизации :ada:`Generic_Swap`.

Пример: Обратный порядок элементов
----------------------------------

Предыдущий пример с алгоритмом обмена двух значений является одним
из простейших примеров использования настраиваемых модулей. Теперь мы
изучим алгоритм обращения элементов массива. Во-первых, давайте
начнем с версии алгоритма без использования настраиваемых модулей,
разработав версию конкретно для типа :ada:`Color`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Test_Non_Generic_Reverse_Colors

    package Colors is

       type Color is (Black, Red, Green,
                      Blue, White);

       type Color_Array is
         array (Integer range <>) of Color;

       procedure Reverse_It (X : in out Color_Array);

    end Colors;

    package body Colors is

       procedure Reverse_It (X : in out Color_Array) is
       begin
          for I in X'First ..
                   (X'Last + X'First) / 2 loop
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
    with Colors;      use Colors;

    procedure Test_Non_Generic_Reverse_Colors is

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

    end Test_Non_Generic_Reverse_Colors;

Процедура :ada:`Reverse_It` принимает массив цветов, начинает с обмена
первого и последнего элементов массива и продолжает делать это со
следующими элементами последовательно, пока не достигнет середины массива. В
этот момент весь массив будет перевернут, как мы видим из выходных
данных тестовой программы.

Чтобы абстрагироваться от этой процедуры, мы объявляем формальные типы
для трех элементов алгоритма:

-  тип компоненты массива (в примере - тип :ada:`Color`)

-  диапазон, используемый для массива (в примере - целочисленный диапазон)

-  фактический тип массива (в примере - тип :ada:`Color_Array`)

Это настраиваемая версия алгоритма:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Test_Reverse_Colors

    generic
       type T is private;
       type Index is range <>;
       type Array_T is
         array (Index range <>) of T;
    procedure Generic_Reverse (X : in out Array_T);

    procedure Generic_Reverse (X : in out Array_T) is
    begin
       for I in X'First ..
                (X'Last + X'First) / 2 loop
          declare
             Tmp     : T;
             X_Left  : T
               renames X (I);
             X_Right : T
               renames X (X'Last + X'First - I);
          begin
             Tmp     := X_Left;
             X_Left  := X_Right;
             X_Right := Tmp;
          end;
       end loop;
    end Generic_Reverse;

    with Generic_Reverse;

    package Colors is

       type Color is (Black, Red, Green,
                      Blue, White);

       type Color_Array is
         array (Integer range <>) of Color;

       procedure Reverse_It is new
         Generic_Reverse (T       => Color,
                          Index   => Integer,
                          Array_T => Color_Array);

    end Colors;

    with Ada.Text_IO; use Ada.Text_IO;
    with Colors;      use Colors;

    procedure Test_Reverse_Colors is

       My_Colors : Color_Array (1 .. 5) :=
         (Black, Red, Green, Blue, White);

    begin
       for C of My_Colors loop
          Put_Line ("My_Color: "
                    & Color'Image (C));
       end loop;

       New_Line;
       Put_Line ("Reversing My_Color...");
       New_Line;
       Reverse_It (My_Colors);

       for C of My_Colors loop
          Put_Line ("My_Color: "
                    & Color'Image (C));
       end loop;

    end Test_Reverse_Colors;

Как упоминалось выше, мы выделили три параметра алгоритма:

-  тип :ada:`T` абстрагирует элементы массива

-  тип :ada:`Index` абстрагирует диапазон, используемый для массива

-  тип :ada:`Array_T` абстрагирует тип массива и использует формальные
   объявления типов :ada:`T` и :ada:`Index`.

Пример: Тестовое приложение
---------------------------

В предыдущем примере мы сосредоточились только на абстрагировании
самого алгоритма обращения массива. Однако мы могли бы аналогично
абстрагировать наше небольшого тестового приложения. Это может
быть полезно, если мы, например, решим протестировать другие
процедуры, меняющие элементы массива.

Чтобы сделать это, мы снова должны выбрать элементы для
абстрагирования. Поэтому мы объевляем следующие формальные
параметры:

-  :ada:`S`: строка, содержащая имя массива

-  функция :ada:`Image`, преобразующая элемент типа :ada:`T` в строку

-  процедура :ada:`Test`, которая выполняет некоторую операцию с массивом

Обратите внимание, что :ada:`Image` и :ada:`Test` являются примерами
формальных подпрограмм, а :ada:`S` - примером формального объекта.

Вот версия тестового приложения, использующего общую процедуру
:ada:`Perform_Test`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Generics.Test_Reverse_Colors_2

    generic
       type T is private;
       type Index is range <>;
       type Array_T is
         array (Index range <>) of T;
    procedure Generic_Reverse (X : in out Array_T);

    procedure Generic_Reverse (X : in out Array_T) is
    begin
       for I in X'First ..
                (X'Last + X'First) / 2 loop
          declare
             Tmp     : T;
             X_Left  : T
               renames X (I);
             X_Right : T
               renames X (X'Last + X'First - I);
          begin
             Tmp     := X_Left;
             X_Left  := X_Right;
             X_Right := Tmp;
          end;
       end loop;
    end Generic_Reverse;

    generic
       type T is private;
       type Index is range <>;
       type Array_T is
         array (Index range <>) of T;
       S : String;
       with function Image (E : T) return String is <>;
       with procedure Test (X : in out Array_T);
    procedure Perform_Test (X : in out Array_T);

    with Ada.Text_IO;  use Ada.Text_IO;

    procedure Perform_Test (X : in out Array_T) is
    begin
       for C of X loop
          Put_Line (S & ": " & Image (C));
       end loop;

       New_Line;
       Put_Line ("Testing " & S & "...");
       New_Line;
       Test (X);

       for C of X loop
          Put_Line (S & ": " & Image (C));
       end loop;
    end Perform_Test;

    with Generic_Reverse;

    package Colors is

       type Color is (Black, Red, Green,
                      Blue, White);

       type Color_Array is
         array (Integer range <>) of Color;

       procedure Reverse_It is new
         Generic_Reverse (T       => Color,
                          Index   => Integer,
                          Array_T => Color_Array);

    end Colors;

    with Colors;       use Colors;
    with Perform_Test;

    procedure Test_Reverse_Colors is

       procedure Perform_Test_Reverse_It is new
         Perform_Test (T       => Color,
                       Index   => Integer,
                       Array_T => Color_Array,
                       S       => "My_Color",
                       Image   => Color'Image,
                       Test    => Reverse_It);

       My_Colors : Color_Array (1 .. 5) :=
         (Black, Red, Green, Blue, White);

    begin
       Perform_Test_Reverse_It (My_Colors);
    end Test_Reverse_Colors;

В этом примере создается процедура, :ada:`Perform_Test_Reverse_It`
как экземпляр настраиваемой процедуры (:ada:`Perform_Test`).
Обратите внимание, что:

-  Для формальной функции :ada:`Image` мы используем атрибут :ada:`'Image`
   типа :ada:`Color`

-  Для формальной процедуры тестирования :ada:`Test` мы ссылаемся на процедуру
   :ada:`Reverse_Array` из пакета.
