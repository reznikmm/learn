Сильно типизированный язык
==========================

.. include:: ../../global.txt

Ada ‑ это строго типизированный язык. В этом отношении интересно то,
что сильная статическая типизация становится все более популярной и
набирает популярность в дизайне языков программирования благодаря
таким факторам, как рост функционального программирования со
статической типизацией, большой толчок со стороны исследовательского
сообщества в области типизации и многие практические языки с сильными
системами типов.

.. _WhatIsAType:

Что такое тип?
--------------

В статически типизированных языках тип в основном (но не только)
является конструкцией *времени компиляции*. Это конструкция,
обеспечивающая инварианты поведения программы. Инварианты ‑ это
неизменяемые свойства, которые сохраняются для всех переменных данного
типа. Их применение гарантирует, например, что переменные типа данных
никогда не будут иметь недопустимых значений.

Тип используется для описания *объектов*, которыми управляет программа
(объект является переменной или константой). Цель состоит в том, чтобы
классифицировать объекты по тому, что можно сделать с ними (т.е.
разрешенные операции), и таким образом можно определить правильность
значений объектов.

Целочисленные типы ‑ Integers
-----------------------------

Хорошей особенностью Ada является то, что вы можете определить свои
собственные целочисленные типы, основываясь на требованиях вашей
программы (т.е. диапазон значений, которые имеют смысл). Фактически
определительный механизм, который предоставляет Ada, образует
семантическую основу для предопределённых целочисленных типов.
Никакого «магического» встроенного типа в этом отношении нет, что не
похоже на большинство языков, и, возможно, очень элегантно.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Integer_Type_Example is
       --  Declare a signed integer type,
       --  and give the bounds
       type My_Int is range -1 .. 20;
       --                         ^ High bound
       --                   ^ Low bound

       --  Like variables, type declarations can
       --  only appear in declarative regions.
    begin
       for I in My_Int loop
          Put_Line (My_Int'Image (I));
          --              ^ 'Image attribute
          --                converts a value
          --                to a String.
       end loop;
    end Integer_Type_Example;

Этот пример иллюстрирует объявление целочисленного типа со знаком, и
несколько вещей, которые мы можем сделать с ними.

Каждое объявление типа в Ada начинается с ключевого слова :ada:`type`
(кроме :ref:`типов задач <TaskTypes>`). После ключевого слова
типа мы можем видеть диапазон,
который очень похож на диапазоны, которые мы используем в циклах for,
которые определяют нижнюю и верхнюю границы типа. Каждое целое число
во включенном диапазоне границ является допустимым значением для типа.

.. admonition:: Целочисленные типы Ada

    В Ada целочисленный тип задается не в терминах его машинного
    представления, а скорее его диапазоном. Затем компилятор выберет
    наиболее подходящее представление.

Еще один момент, который следует отметить в приведенном выше примере,
‑ это выражение :ada:`My_Int'Image (I)`. Обозначение атрибута
:ada:`Name'Attribute (необязательные параметры)` используется для того,
что в Ada называется атрибутом.
Атрибут ‑ это встроенная операция над типом, значением или какой-либо
другой программной сущностью. Доступ к нему осуществляется с помощью
символа :ada:`'` (апостроф ASCII).

Ada имеет несколько типов, доступных как "встроенные модули"; :ada:`Integer` ‑
один из них. Вот как тип целых чисел :ada:`Integer` может быть определен
для типичного процессора:

.. code-block:: ada

    type Integer is
      range -(2 ** 31) .. +(2 ** 31 - 1);

:ada:`**` является оператором экспоненты, что означает, что первое допустимое
значение для целого числа типа :ada:`Integer` равно -2\ :sup:`31`, а последнее
допустимое значение равно 2\ :sup:`31` - 1.

Ada не требует диапазона встроенного типа Integer. Реализация для
16-битного целевого процессора, вероятно, выберет диапазон
от - 2\ :sup:`15` до 2\ :sup:`15` - 1.

Семантика операций
~~~~~~~~~~~~~~~~~~

В отличие от некоторых других языков, Ada требует, чтобы операции с
целыми числами проверялись на переполнение.

.. code-block:: ada
    :class: ada-run-expect-failure

    procedure Main is
       A : Integer := Integer'Last;
       B : Integer;
    begin
       B := A + 5;
       --  This operation will overflow, eg. it
       --  will raise an exception at run time.
    end Main;

Существует два типа проверок переполнения:

 - Переполнение на уровне процессора, когда результат операции превышает
   максимальное значение (или меньше минимального значения), которое
   может быть представлено в хранилище, зарезервированном для объекта
   данного типа, и

 - Переполнение на уровне типа, если результат операции выходит за
   пределы диапазона, определенного для типа.

В основном по соображениям эффективности, в то время как переполнение
процессорного уровня всегда приводит к исключению, переполнение уровня
типа будет проверяться только на определенных границах, таких как
назначение:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type My_Int is range 1 .. 20;
       A : My_Int := 12;
       B : My_Int := 15;
       M : My_Int := (A + B) / 2;
       --  No overflow here, overflow checks
       --  are done at specific boundaries.
    begin
       for I in 1 .. M loop
          Put_Line ("Hello, World!");
       end loop;
       --  Loop body executed 13 times
    end Main;

Переполнение уровня типа будет проверяться только в определенных
точках выполнения. Результат, как мы видим выше, состоит в том, что у
вас может быть операция, которая переполняется в промежуточном
вычислении, но никаких исключений не будет, потому что конечный
результат не переполняется.

Беззнаковые типы
----------------

Ada также поддерживает целочисленные типы без знака. На языке Ada они
называются *модульными* типами. Причина такого обозначения связана с их
поведением в случае переполнения: они просто "оборачиваются", как если
бы была применена операция по модулю.

Для модульных типов машинного размера, например модуля 2\ :sup:`32`, это
имитирует наиболее распространенное поведение реализации беззнаковых
типов. Однако преимущество Ada в том, что модуль более общий:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Mod_Int is mod 2 ** 5;
       --              ^ Range is 0 .. 31

       A : constant Mod_Int := 20;
       B : constant Mod_Int := 15;

       M : constant Mod_Int := A + B;
       --  No overflow here,
       --  M = (20 + 15) mod 32 = 3
    begin
       for I in 1 .. M loop
          Put_Line ("Hello, World!");
       end loop;
    end Main;

В отличие от C/C++, так как такое поведение гарантировано
спецификацией Ada, на неё можно положиться для реализации переносимого
кода. Кроме того, для реализации определенных алгоритмов и структур
данных, таких как
`кольцевые буферы <https://ru.wikipedia.org/wiki/Кольцевой_буфер>`_,
очень полезно иметь возможность использовать обертку на произвольных
границах ‑ модуль не обязательно должен быть степенью 2.

.. _EnumTypes:

Перечисления
------------

Типы перечисления ‑ еще одна особенность системы типов Ada. В отличие
от перечислений C, они *не* являются целыми числами, и каждый новый тип
перечисления несовместим с другими типами перечислений. Типы
перечисления являются частью большего семейства дискретных типов, что
делает их пригодными для использования в определенных ситуациях,
которые мы опишем позже, но один контекст, который мы уже видели, ‑
это оператор case.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Enumeration_Example is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);
       --  An enumeration type
    begin
       for I in Days loop
          case I is
             when Saturday .. Sunday =>
                Put_Line ("Week end!");

             when Monday .. Friday =>
                Put_Line ("Hello on "
                          & Days'Image (I));
                --  'Image attribute, works on
                --  enums too
          end case;
       end loop;
    end Enumeration_Example;

Типы перечисления достаточно мощные, поэтому, в отличие от большинства
языков, они используются для определения стандартного логического
типа:

.. code-block:: ada

    type Boolean is (False, True);

Как упоминалось ранее, каждый "встроенный" тип в Ada определяется с
помощью средств, обычно доступных пользователю.

Типы с плавающей запятой
------------------------

Основные свойства
~~~~~~~~~~~~~~~~~

Как и большинство языков, Ada поддерживает типы с плавающей запятой.
Наиболее часто используемый тип с плавающей запятой ‑ :ada:`Float`:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Demo is
       A : constant Float := 2.5;
    begin
       Put_Line ("The value of A is "
                 & Float'Image (A));
    end Floating_Point_Demo;

Приложение отобразит :ada:`2.5` как значение :ada:`A`.

Язык Ada не определяет точность (количество десятичных цифр в
мантиссе) для Float; на типичной 32-разрядной машине точность будет
равна 6.

Доступны все распространенные операции, которые можно было бы ожидать
для типов с плавающей запятой, включая абсолютное значение и
возведение в степень. Например:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Operations is
       A : Float := 2.5;
    begin
       A := abs (A - 4.5);
       Put_Line ("The value of A is "
                 & Float'Image (A));

       A := A ** 2 + 1.0;
       Put_Line ("The value of A is "
                 & Float'Image (A));
    end Floating_Point_Operations;

Значение :ada:`A` равно :ada:`2.0` после первой операции и :ada:`5.0` после второй операции.

В дополнение к :ada:`Float`, реализация Ada может предлагать типы данных с более
высокой точностью, такие как :ada:`Long_Float` и :ada:`Long_Long_Float`. Как и Float, стандарт не указывает
точную точность этих типов: он только гарантирует, что тип :ada:`Long_Float`, например,
имеет хотя бы точность :ada:`Float`. Чтобы гарантировать выполнение определенного
требования к точности, можно определить пользовательские типы с
плавающей запятой, как будет показано в следующем разделе.

Точность типов с плавающей запятой
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ada позволяет пользователю задавать точность для типа с плавающей
запятой, выраженную в виде десятичных цифр. Операции с этими
пользовательскими типами будут иметь, по крайней мере, заданную
точность. Синтаксис простого объявления типа с плавающей запятой:

.. code-block:: ada

    type T is digits <number_of_decimal_digits>;

Компилятор выберет представление с плавающей запятой, поддерживающее
требуемую точность. Например:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Custom_Floating_Types is
       type T3  is digits 3;
       type T15 is digits 15;
       type T18 is digits 18;
    begin
       Put_Line ("T3  requires "
                 & Integer'Image (T3'Size)
                 & " bits");
       Put_Line ("T15 requires "
                 & Integer'Image (T15'Size)
                 & " bits");
       Put_Line ("T18 requires "
                 & Integer'Image (T18'Size)
                 & " bits");
    end Custom_Floating_Types;

В этом примере атрибут «:ada:`'Size`» используется для получения количества битов,
используемых для указанного типа данных. Как видно из этого примера,
компилятор выделяет 32 бита для :ada:`T3`, 64 бита для :ada:`T15` и 128 битов для :ada:`T18`. Сюда
входят как мантисса, так и экспонента.

Количество цифр, указанное в типе данных, также используется в формате
при отображении переменных с плавающей точкой. Например:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Display_Custom_Floating_Types is
       type T3  is digits 3;
       type T18 is digits 18;

       C1 : constant := 1.0e-4;

       A : constant T3  := 1.0 + C1;
       B : constant T18 := 1.0 + C1;
    begin
       Put_Line ("The value of A is "
                 & T3'Image (A));
       Put_Line ("The value of B is "
                 & T18'Image (B));
    end Display_Custom_Floating_Types;

Как и ожидалось, приложение будет отображать переменные в соответствии
с заданной точностью (1.00E + 00 и 1.00010000000000000E + 00).

Диапазон типов с плавающей запятой
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

В дополнение к точности для типа с плавающей запятой можно также
задать диапазон. Синтаксис аналогичен используемому для целочисленных
типов данных ‑ с использованием ключевого слова :ada:`range`. В этом простом
примере создается новый тип с плавающей запятой на основе типа :ada:`Float` для
нормированного диапазона от :ada:`-1.0` до :ada:`1.0`:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Range is
       type T_Norm  is new Float range -1.0 .. 1.0;
       A  : T_Norm;
    begin
       A := 1.0;
       Put_Line ("The value of A is "
                 & T_Norm'Image (A));
    end Floating_Point_Range;

Приложение отвечает за обеспечение того, чтобы переменные этого типа
находились в пределах этого диапазона; в противном случае возникает
исключение. В этом примере :ada:`Constraint_Error` исключения возникает при назначении :ada:`2.0`
переменной :ada:`A`:

.. code-block:: ada
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Range_Exception is
       type T_Norm  is new Float range -1.0 .. 1.0;
       A  : T_Norm;
    begin
       A := 2.0;
       Put_Line ("The value of A is "
                 & T_Norm'Image (A));
    end Floating_Point_Range_Exception;

Диапазоны также могут быть заданы для пользовательских типов с
плавающей запятой. Например:

.. code-block:: ada
    :class: ada-expect-compile-error

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    procedure Custom_Range_Types is
       type T6_Inv_Trig  is
         digits 6 range -Pi / 2.0 .. Pi / 2.0;
    begin
       null;
    end Custom_Range_Types;

В этом примере мы определяем тип под названием :ada:`T6_Inv_Trig`, который имеет
диапазон от −𝜋/2 до 𝜋/2 с минимальной точностью 6 цифр. (:ada:`Pi` определяется
в предопределенном пакете :ada:`Ada.Numerics`.)

Строгая типизация
-----------------

Как отмечалось ранее, язык Ada строго типизирован. В результате разные
типы одного семейства несовместимы друг с другом; значение одного типа
не может быть присвоено переменной другого типа. Например:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Illegal_Example is
       --  Declare two different floating point types
       type Meters is new Float;
       type Miles is new Float;

       Dist_Imperial : Miles;

       --  Declare a constant
       Dist_Metric : constant Meters := 1000.0;
    begin
       --  Not correct: types mismatch
       Dist_Imperial := Dist_Metric * 621.371e-6;
       Put_Line (Miles'Image (Dist_Imperial));
    end Illegal_Example;

Следствием этих правил является то, что в общем случае выражение
«смешанного режима» типа :ada:`2 * 3.0` инициирует ошибку компиляции. В языке, таком
как C или Python, такие выражения становятся действительными
посредством неявных преобразований. В Ada такие преобразования должны
быть явными:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    procedure Conv is
       type Meters is new Float;
       type Miles is new Float;
       Dist_Imperial : Miles;
       Dist_Metric : constant Meters := 1000.0;
    begin
       Dist_Imperial := Miles (Dist_Metric) * 621.371e-6;
       --               ^ Type conversion,
       --                 from Meters to Miles
       --  Now the code is correct

       Put_Line (Miles'Image (Dist_Imperial));
    end Conv;

Конечно, мы, вероятно, не хотим писать код преобразования каждый раз,
когда мы преобразуем метры в мили. Идиоматическим решением в этом
случае было бы ввести функции преобразования вместе с типами.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Conv is
       type Meters is new Float;
       type Miles is new Float;

       --  Function declaration, like procedure
       --  but returns a value.
       function To_Miles (M : Meters) return Miles is
       --                             ^ Return type
       begin
          return Miles (M) * 621.371e-6;
       end To_Miles;

       Dist_Imperial : Miles;
       Dist_Metric   : constant Meters := 1000.0;
    begin
       Dist_Imperial := To_Miles (Dist_Metric);
       Put_Line (Miles'Image (Dist_Imperial));
    end Conv;

Если вы пишете много числового кода, то необходимость явно
предоставлять такие преобразования может показаться обременительным.
Однако такой подход дает определенные преимущества. Примечательно, что
вы можете полагаться на отсутствие неявных преобразований, которые, в
свою очередь, предотвратят некоторые тонкие ошибки.

.. admonition:: На других языках

    В C, например, правила для неявных преобразований не всегда могут быть
    полностью очевидными. Однако в Ada код всегда будет делать именно то,
    что, явно определено программистом. Например:

    .. code-block:: c

        int a = 3, b = 2;
        float f = a / b;

    Этот код будет компилироваться нормально, но результатом :c:`f` будет 1.0
    вместо 1.5, потому что компилятор сгенерирует целочисленное деление
    (три, разделенное на два), что приведет к единице. Разработчик
    программного обеспечения должен знать о проблемах преобразования
    данных и использовать соответствующее приведение типов:

    .. code-block:: c

        int a = 3, b = 2;
        float f = (float)a / b;

    В исправленном примере компилятор преобразует обе переменные в
    соответствующее представление с плавающей запятой перед выполнением
    деления. Это даст ожидаемый результат.

    Этот пример очень прост, и опытные разработчики C, вероятно, заметят и
    исправят его, прежде чем это создаст большие проблемы. Однако в более
    сложных приложениях, где объявление типа не всегда видно ‑ например,
    при ссылке на элементы структуры :c:`struct`, ‑ эта ситуация может не всегда быть
    очевидной и быстро привести к дефектам программного обеспечения,
    которые может быть сложнее обнаружить.

    Компилятор Ada, напротив, всегда будет отклонять код, который
    смешивает переменные с плавающей запятой и целочисленные переменные
    без явного преобразования. Следующий код Ada, основанный на ошибочном
    примере в C, не будет компилироваться:

    .. code-block:: ada
        :class: ada-expect-compile-error

        procedure Main is
           A : Integer := 3;
           B : Integer := 2;
           F : Float;
        begin
           F := A / B;
        end Main;

    Строка кода нарушитель должна быть изменена на :ada:`F := Float (A) / Float (B);` для принятия
    компилятором.

-  Вы можете использовать строгую типизацию Ada, чтобы обеспечить
   соблюдение инвариантов в вашем коде, как в приведенном выше примере:
   поскольку Мили и метры ‑ это два разных типа, вы не можете ошибочно
   преобразовать экземпляр одного в экземпляр другого.

Производные типы
----------------

В Ada можно создавать новые типы на основе существующих. Это очень
полезно: вы получаете тип, который имеет те же свойства, что и
некоторый существующий тип, но рассматривается как отдельный тип в
интересах сильной типизации.

.. code-block:: ada
    :class: ada-expect-compile-error

    procedure Main is
       --  ID card number type,
       --  incompatible with Integer.
       type Social_Security_Number is new Integer
         range 0 .. 999_99_9999;
       --      ^ Since a SSN has 9 digits
       --        max., and cannot be
       --        negative, we enforce
       --        a validity constraint.

       SSN : Social_Security_Number :=
         555_55_5555;
       --   ^ You can put underscores as
       --     formatting in any number.

       I   : Integer;

       --  The value -1 below will cause a
       --  runtime error and a compile time
       --  warning with GNAT.
       Invalid : Social_Security_Number := -1;
    begin
       --  Illegal, they have different types:
       I := SSN;

       --  Likewise illegal:
       SSN := I;

       --  OK with explicit conversion:
       I := Integer (SSN);

       --  Likewise OK:
       SSN := Social_Security_Number (I);
    end Main;

Тип :ada:`Social_Security`, как говорят, является *производным типом*;
его *родительский тип* ‑ Integer.

Как показано в этом примере, вы можете уточнить допустимый диапазон
при определении производного скалярного типа (такого как целое число,
число с плавающей запятой и перечисление).

Синтаксис перечислений использует синтаксис :ada:`range <диапазон>`:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       type Weekend_Days is new
         Days range Saturday .. Sunday;
       --  New type, where only Saturday and Sunday
       --  are valid literals.
    begin
       null;
    end Greet;

Подтипы
-------

Как мы начинаем видеть, типы могут использоваться в Ada для наложения
ограничений на допустимый диапазон значений. Однако иногда требуется
наложить ограничения на некоторые значения, оставаясь в пределах
одного типа. Здесь вступают в действие подтипы. Подтип не вводит новый
тип.

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       --  Declaration of a subtype
       subtype Weekend_Days is
         Days range Saturday .. Sunday;
       --     ^ Constraint of the subtype

       M : Days := Sunday;

       S : Weekend_Days := M;
       --  No error here, Days and Weekend_Days
       --  are of the same type.
    begin
       for I in Days loop
          case I is
             --  Just like a type, a subtype can
             --  be used as a range
             when Weekend_Days =>
                Put_Line ("Week end!");
             when others =>
                Put_Line ("Hello on "
                          & Days'Image (I));
          end case;
       end loop;
    end Greet;

Несколько подтипов предопределены в стандартном пакете Ada и
автоматически доступны вам:

.. code-block:: ada

    subtype Natural  is Integer range 0 .. Integer'Last;
    subtype Positive is Integer range 1 .. Integer'Last;

Хотя подтипы типа статически совместимы друг с другом, ограничения
применяются во время выполнения: если вы нарушите ограничение подтипа,
возникнет исключение.

.. code-block:: ada
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       subtype Weekend_Days is
         Days range Saturday .. Sunday;

       Day     : Days := Saturday;
       Weekend : Weekend_Days;
    begin
       Weekend := Day;
       --         ^ Correct: Same type, subtype
       --           constraints are respected
       Weekend := Monday;
       --         ^ Wrong value for the subtype
       --           Compiles, but exception at runtime
    end Greet;

.. _SubtypeAliases:

Подтипы в качестве псевдонимов типов
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ранее мы видели, что мы можем создавать новые типы, объявляя
:ada:`type Miles is new Float`. Мы также могли бы создать псевдонимы типов,
которые генерируют альтернативные имена ‑ псевдонимы ‑ для известных типов.
Следует отметить, что *псевдонимы* типов иногда называются
*синонимами типов*.

Мы достигаем этого в Ada, используя подтипы без новых ограничений.
Однако в этом случае мы не получаем всех преимуществ, а именно строгой
проверки типов в Ada. Перепишем пример, используя псевдонимы типов:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Undetected_Imperial_Metric_Error is
       --  Declare two type aliases
       subtype Meters is Float;
       subtype Miles is Float;

       Dist_Imperial : Miles;

       --  Declare a constant
       Dist_Metric : constant Meters := 100.0;
    begin
       --  No conversion to Miles type required:
       Dist_Imperial := (Dist_Metric * 1609.0) / 1000.0;

       --  Not correct, but undetected:
       Dist_Imperial := Dist_Metric;

       Put_Line (Miles'Image (Dist_Imperial));
    end Undetected_Imperial_Metric_Error;

В приведенном выше примере тот факт, что и Метры (:ada:`Meters`), и Мили (:ada:`Miles`)
являются подтипами :ada:`Float`, позволяет нам смешивать переменные обоих типов
без преобразования типов. Это, однако, может привести к всевозможным
ошибкам в программировании, которых мы хотели бы избежать, как мы
можем видеть в необнаруженной ошибке, выделенной в приведенном выше
коде. В этом примере ошибка в присвоении значения в метрах переменной,
предназначенной для хранения значений в милях, остается
необнаруженной, поскольку и Метры (:ada:`Meters`), и Мили (:ada:`Miles`) являются подтипами :ada:`Float`.
Поэтому рекомендуется использовать строгую типизацию ‑ через тип X ‑
это новый Y (:ada:`type X is new Y`) ‑ для случаев, подобных приведенному выше.

Однако существует много ситуаций, когда псевдонимы типов полезны.
Например, в приложении, которое использует типы с плавающей запятой в
нескольких контекстах, мы могли бы использовать псевдонимы типов,
чтобы указать дополнительное значение типов или избежать длинных имен
переменных. Например, вместо того, чтобы писать:

.. code-block:: ada

    Paid_Amount, Due_Amount : Float;



.. code-block:: ada

    subtype Amount is Float;

    Paid, Due : Amount;

.. admonition:: На других языках

    Например, в C для создания псевдонима типа можно использовать
    объявление :c:`typedef`. Например:

    .. code-block:: c

        typedef float meters;

    Это соответствует описанию, которое мы видели выше, используя подтипы.
    Другие языки программирования включают эту концепцию аналогичными
    способами. Например:

        - C++: ``using meters = float;``
        - Swift: ``typealias Meters = Double``
        - Kotlin: ``typealias Meters = Double``
        - Haskell: ``type Meters = Float``

Однако следует отметить, что подтипы в Ada соответствуют псевдонимам
типов, если и только если они не имеют новых ограничений. Таким
образом, если добавить новое ограничение к описанию подтипа, у нас
больше не будет псевдонима типа. Например, следующее объявление
*не может* считаться псевдонимом типа :ada:`Float`:

.. code-block:: ada

    subtype Meters is Float range 0.0 .. 1_000_000.0;

Рассмотрим другой пример:

.. code-block:: ada

    subtype Degree_Celsius is Float;

    subtype Liquid_Water_Temperature is
      Degree_Celsius range 0.0 .. 100.0;

    subtype Running_Water_Temperature is
      Liquid_Water_Temperature;

В этом примере :ada:`Liquid_Water_Temperature` не является псевдонимом :ada:`Degree_Celsius`, поскольку добавляет новое
ограничение, которое не было частью объявления :ada:`Degree_Celsius`. Однако здесь есть два
псевдонима типа:

-  :ada:`Degree_Celsius` является псевдонимом :ada:`Float`;

-  :ada:`Running_Water_Temperature` является псевдонимом :ada:`Liquid_Water_Temperature`, даже если сам :ada:`Liquid_Water_Temperature` имеет ограниченный диапазон.
