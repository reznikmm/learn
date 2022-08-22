Сильно типизированный язык
==========================

.. include:: ../../global.txt

Ада - это строго типизированный язык. Удивительно, как она
современна в этом:
сильная статическая типизация становится все более популярной
в дизайне языков программирования,
если судить по таким факторам, как
развитие функционального программирования со статической типизацией,
прилагаемые усилия в области типизации со стороны исследовательского
сообщества и
появление множества практических языков с сильными
системами типов.

.. _Intro_Ada_Ru_WhatIsAType:

Что такое тип?
--------------

В статически типизированных языках тип в основном (но не только)
является конструкцией *времени компиляции*. Это конструкция,
обеспечивающая инварианты поведения программы. Инварианты - это
неизменяемые свойства, которые сохраняются для всех переменных данного
типа. Их применение гарантирует, например, что значения переменных
данного типа никогда не будут иметь недопустимых значений.

Тип используется для описания *объектов*, которыми управляет программа
(объект это переменная или константа). Цель состоит в том, чтобы
классифицировать объекты по тому, что можно с ними сделать (т.е.
по разрешенным операциям), и, таким образом, судить о правильности
значений объектов.

Целочисленные типы - Integers
-----------------------------

Приятной возможностью языка Ада является то, что вы можете определить свои
собственные целочисленные типы, основываясь на требованиях вашей
программы (т.е. на диапазоне значений, который имеет смысл). Фактически
механизм определения типов, который предоставляет Ада, лежит в
основе предопределённых целочисленных типов.
Таким образом, в языке нет «магических» встроенных типов, как в
большинстве других языков, и это, пожалуй, очень элегантно.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Integer_Type_Example

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
несколько моментов, связанных с его использованием.

Каждое объявление типа в Аде начинается с ключевого слова :ada:`type`
(кроме :ref:`задачных типов <Intro_Ada_Ru_TaskTypes>`). После ключевого слова
мы можем видеть определение нижней и верхней границ типа
в виде диапазона,
который очень похож на диапазоны используемые в циклах :ada:`for`.
Любое целое число
этого диапазона является допустимым значением для данного типа.

.. admonition:: Целочисленные типы Ада

    В Аде целочисленный тип задается не в терминах его машинного
    представления, а скорее его диапазоном. Затем компилятор сам
    выберет наиболее подходящее представление.

Еще один момент, который следует отметить в приведенном выше примере,
- это выражение :ada:`My_Int'Image (I)`. Обозначение вида
:ada:`Name'Attribute (необязательные параметры)` используется для того,
что в Аде называется атрибутом.
Атрибут - это встроенная операция над типом, значением или какой-либо
другой программной сущностью. Доступ к нему осуществляется с помощью
символа :ada:`'` (апостроф в ASCII).

Ада имеет несколько "встроенных" типов; :ada:`Integer` -
один из них. Вот как тип целых чисел :ada:`Integer` может быть определен
для типичного процессора:

.. code-block:: ada

    type Integer is
      range -(2 ** 31) .. +(2 ** 31 - 1);

Знак :ada:`**` обозначает возведение в степень, в итоге, первое допустимое
значение для типа :ada:`Integer` равно -2\ :sup:`31`, а последнее
допустимое значение равно 2\ :sup:`31` - 1.

Ада не регламентирует диапазон встроенного типа :ada:`Integer`.
Реализация для 16-битного целевого процессора, вероятно, выберет диапазон
от - 2\ :sup:`15` до 2\ :sup:`15` - 1.

Семантика операций
~~~~~~~~~~~~~~~~~~

В отличие от некоторых других языков, Ада требует, чтобы операции с
целыми числами контролировали переполнение.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Overflow_Check
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
   может поместиться в машинном представлении объекта
   данного типа, и

 - Переполнение на уровне типа, если результат операции выходит за
   пределы диапазона, определенного для типа.

В основном по соображениям эффективности, переполнение уровня
типа будет проверяться лишь в определенные моменты, такие как присваивание
значения, тогда как, переполнение низкого уровня всегда приводит к
возбуждению исключения:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Overflow_Check_2

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
вас может быть операция в промежуточном вычислении, которая переполняется,
но никаких исключений не будет, пока конечный результат не вызовет
переполняения.

Беззнаковые типы
----------------

Ада также поддерживает целочисленные типы без знака. На языке Ада они
называются *модульными* типами. Причина такого обозначения связана с их
поведением в случае переполнения: они просто "заварачиваются", как если
бы была применена операция по модулю.

Для модульных типов машинного размера, например модуля 2\ :sup:`32`, это
имитирует наиболее распространенное поведение реализации беззнаковых
типов. Однако преимущество Ада в том, что модуль может быть произвольным:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Unsigned_Types

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

В отличие от C/C++, такое поведение гарантировано спецификацией языка
Ада и на него можно положиться при создании переносимого
кода. Кроме того, для реализации определенных алгоритмов и структур
данных, таких как
`кольцевые буферы <https://ru.wikipedia.org/wiki/Кольцевой_буфер>`_,
очень удобно иметь возможность использовать эффект "заворачивания"
на произвольных границах, ведь модуль не обязательно должен быть степенью 2.

.. _Intro_Ada_Ru_EnumTypes:

Перечисления
------------

Перечислимые типы - еще одна особенность системы типов в Аде. В отличие
от перечислений C, они *не* являются целыми числами, и каждый новый
перечислимый тип несовместим с другими перечислимыми типами.
Перечислимые типы являются частью большего семейства дискретных типов, что
делает их пригодными для использования в определенных ситуациях,
которые мы опишем позже, но один контекст, с которым мы уже встречались, -
это оператор case.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Enumeration_Example

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

Как упоминалось ранее, каждый "встроенный" тип в Аде определяется с
помощью средств, обычно доступных пользователю.

Типы с плавающей запятой
------------------------

Основные свойства
~~~~~~~~~~~~~~~~~

Как и большинство языков, Ада поддерживает типы с плавающей запятой.
Наиболее часто используемый тип с плавающей запятой - :ada:`Float`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Floating_Point_Demo

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Floating_Point_Demo is
       A : constant Float := 2.5;
    begin
       Put_Line ("The value of A is "
                 & Float'Image (A));
    end Floating_Point_Demo;

Приложение отобразит :ada:`2.5` как значение :ada:`A`.

Язык Ада не регламентирует точность (количество десятичных цифр в
мантиссе) для Float; на типичной 32-разрядной машине точность будет
равна 6.

Доступны все общепринятые операции, которые можно было бы ожидать
для типов с плавающей запятой, включая получение абсолютного значения и
возведение в степень. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Floating_Point_Operations

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

В дополнение к :ada:`Float`, реализация Ада может предлагать типы данных с более
высокой точностью, такие как :ada:`Long_Float` и :ada:`Long_Long_Float`.
Как и для Float, стандарт не указывает требуемую точность этих типов:
он только гарантирует, что тип :ada:`Long_Float`, например,
имеет точность не хуже :ada:`Float`. Чтобы гарантировать необходимую
точность, можно определить свой пользовательский тип с
плавающей запятой, как будет показано в следующем разделе.

Точность типов с плавающей запятой
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ада позволяет пользователю определить тип с плавающей
запятой с заданной точностью, выраженной в десятичных знаках.
Все операции этого типа будут иметь, по крайней мере, заданную
точность. Синтаксис простого объявления типа с плавающей запятой:

.. code-block:: ada

    type T is digits <number_of_decimal_digits>;

Компилятор выберет представление с плавающей запятой, поддерживающее
требуемую точность. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Custom_Floating_Types

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

В этом примере атрибут «:ada:`'Size`» используется для получения количества бит,
используемых для указанного типа данных. Как видно из этого примера,
компилятор выделяет 32 бита для :ada:`T3`, 64 бита для :ada:`T15` и 128 битов
для :ada:`T18`. Сюда входят как мантисса, так и экспонента.

Количество цифр, указанное в типе данных, также используется в формате
при отображении переменных с плавающей точкой. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Display_Custom_Floating_Types

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

Диапазон значений для типов с плавающей запятой
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

В дополнение к точности для типа с плавающей запятой можно также
задать диапазон. Синтаксис аналогичен записи для целочисленных
типов данных |mdash| с использованием ключевого слова :ada:`range`.
В этом простом примере создается новый тип с плавающей запятой на
основе типа :ada:`Float` с диапазоном от :ada:`-1.0` до :ada:`1.0`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Floating_Point_Range

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
исключение. В следующем примере :ada:`Constraint_Error` исключения
возникает при присваивании значения :ada:`2.0` переменной :ada:`A`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Floating_Point_Range_Exception
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

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Custom_Range_Types

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

Как отмечалось ранее, язык Ада строго типизирован. В результате разные
типы одного семейства несовместимы друг с другом; значение одного типа
не может быть присвоено переменной другого типа. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Imperial_Metric_Error
    :class: ada-expect-compile-error

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
как C или Python, такие выражения допустимы благодаря
неявным преобразованиям типов. В Ада такие преобразования должны
быть явными:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Imperial_Metric

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
случае считается введение функции преобразования вместе с типами.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Imperial_Metric_Func

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

Если вы пишете код, где много вычислений, то необходимость явных
преобразований может показаться обременительным.
Однако такой подход дает определенные преимущества. Ведь
вы можете полагаться на отсутствие неявных преобразований, которые, в
свою очередь, могут привести к тяжело обнаружимым ошибкам.

.. admonition:: На других языках

    В C, например, правила для неявных преобразований не всегда могут быть
    полностью очевидными. Однако в Аде код всегда будет делать именно то,
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
    деления. Что даст ожидаемый результат.

    Этот пример очень прост, и опытные разработчики C, вероятно, заметят и
    исправят его, прежде чем это создаст большие проблемы. Однако в более
    сложных приложениях, где объявление типа не всегда видно - например,
    при ссылке на элементы структуры :c:`struct`, - эта ситуация может не всегда быть
    очевидной и быстро привести к дефектам программного обеспечения,
    которые обнаружить может быть сложнее.

    Компилятор Ада, напротив, всегда будет отклонять код, который
    смешивает переменные с плавающей запятой и целочисленные переменные
    без явного преобразования. Следующий Ада код, основанный на ошибочном
    примере в C, не будет компилироваться:

    .. code:: ada compile_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Implicit_Cast
        :class: ada-expect-compile-error

        procedure Main is
           A : Integer := 3;
           B : Integer := 2;
           F : Float;
        begin
           F := A / B;
        end Main;

    Строка с ошибкой должна быть изменена на :ada:`F := Float (A) / Float (B);`.

-  Вы можете использовать строгую типизацию Ада, чтобы обеспечить
   соблюдение инвариантов в вашем коде, как в приведенном выше примере:
   поскольку мили и метры - это два разных типа, вы не можете случайно
   использовать значения одного типа вместо другого.

Производные типы
----------------

В Ада можно создавать новые типы на основе существующих. Это очень
полезно: вы получаете тип, который имеет те же свойства, что и
некоторый существующий тип, но ведет себя как отдельный тип в
соответствии с правилами сильной типизации.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Derived_Types
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
его *родительский тип* - Integer.

Как показано в этом примере, вы можете уточнить допустимый диапазон значений
при определении производного скалярного типа (такого как целое число,
число с плавающей запятой и перечисление).

Синтаксис перечислений использует синтаксис :ada:`range <диапазон>`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Days

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

Вышеизложенное может привести нас к идее, что типы в Аде могут быть
использованы для наложения ограничений на диапазон допустимый значений.
Но иногда бывает нужно ограничить значения оставаясь в пределах
одного типа. Здесь приходят на помощь подтипы. Подтип не вводит новый
тип.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Days_Subtype

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

Несколько подтипов предопределены в стандартном пакете Ада и
автоматически доступны вам:

.. code-block:: ada

    subtype Natural  is Integer range 0 .. Integer'Last;
    subtype Positive is Integer range 1 .. Integer'Last;

Хотя подтипы одного типа статически совместимы друг с другом, ограничения
проверяются во время выполнения: если вы нарушите ограничение подтипа,
будет возбуждено исключение.

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Days_Subtype_Error
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

.. _Intro_Ada_Ru_SubtypeAliases:

Подтипы в качестве псевдонимов типов
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ранее мы видели, что мы можем создавать новые типы, объявляя
:ada:`type Miles is new Float`. Но нам также может потребоваться переименовать
тип, просто чтобы ввести альтернативное имя-псевдоним для
существующего типа.
Следует отметить, что *псевдонимы* типов иногда называются
*синонимами* типов.

В Аде это делается с помощью подтипов без новых ограничений.
Однако в этом случае мы не получаем всех преимуществ строгой
типизации Ады. Перепишем пример, используя псевдонимы типов:

.. code:: ada run_button project=Courses.Intro_To_Ada.Strongly_Typed_Language.Undetected_Imperial_Metric_Error

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

В приведенном выше примере тот факт, что и метры (:ada:`Meters`), и мили (:ada:`Miles`)
являются подтипами :ada:`Float`, позволяет нам смешивать переменные обоих типов
без преобразования типов. Это, однако, может привести к всевозможным
ошибкам в программировании, которых мы стремимся избежать, как
можно видеть в необнаруженной ошибке, выделенной в приведенном выше
коде. В этом примере ошибка в присвоении значения в метрах переменной,
предназначенной для хранения значений в милях, остается
необнаруженной, поскольку и метры (:ada:`Meters`), и мили (:ada:`Miles`)
являются подтипами :ada:`Float`.
Поэтому, для случаев, подобных приведенному выше, рекомендуется использовать
строгую типизацию, опеделив тип X производным от типа Y (:ada:`type X is new Y`).

Однако существует много ситуаций, где псевдонимы типов полезны.
Например, в приложении, которое использует типы с плавающей запятой в
нескольких контекстах, мы могли бы использовать псевдонимы типов,
чтобы уточнить преднозначение или избежать длинных имен
переменных. Например, вместо того, чтобы писать:

.. code-block:: ada

    Paid_Amount, Due_Amount : Float;

Мы можем написать:

.. code-block:: ada

    subtype Amount is Float;

    Paid, Due : Amount;

.. admonition:: На других языках

    Например, в C для создания псевдонима типа можно использовать
    объявление :c:`typedef`. Например:

    .. code-block:: c

        typedef float meters;

    Это соответствует определению подтипа без ограничений, которое мы видели
    выше.
    Другие языки программирования вводят эту концепцию аналогичными
    способами. Например:

        - C++: ``using meters = float;``
        - Swift: ``typealias Meters = Double``
        - Kotlin: ``typealias Meters = Double``
        - Haskell: ``type Meters = Float``

Однако следует отметить, что подтипы в Аде соответствуют псевдонимам
типов, если и только если они не вводят новых ограничений. Таким
образом, если добавить новое ограничение к описанию подтипа, у нас
больше не будет псевдонима типа. Например, следующее объявление
*не может* считаться синонимом типа :ada:`Float`:

.. code-block:: ada

    subtype Meters is Float range 0.0 .. 1_000_000.0;

Рассмотрим другой пример:

.. code-block:: ada

    subtype Degree_Celsius is Float;

    subtype Liquid_Water_Temperature is
      Degree_Celsius range 0.0 .. 100.0;

    subtype Running_Water_Temperature is
      Liquid_Water_Temperature;

В этом примере :ada:`Liquid_Water_Temperature` не является псевдонимом
:ada:`Degree_Celsius`, поскольку добавляет новое
ограничение, которое не было частью объявления :ada:`Degree_Celsius`.
Однако здесь есть два псевдонима типа:

-  :ada:`Degree_Celsius` является псевдонимом :ada:`Float`;

-  :ada:`Running_Water_Temperature` является псевдонимом
   :ada:`Liquid_Water_Temperature`, даже если сам
   :ada:`Liquid_Water_Temperature` имеет ограниченный диапазон.
