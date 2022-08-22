Подробнее о типах
=================

.. _Intro_Ada_Ru_Aggregates:

.. include:: ../../global.txt

Агрегаты: краткая информация
----------------------------

До сих пор мы говорили об агрегатах довольно мало и видели лишь ряд
примеров. Теперь мы рассмотрим эту конструкцию более подробно.

Агрегат в Аде фактически является литералом составного типа.
Это очень мощная форма записи во многих случаях позволяет избежать
написания процедурного кода для инициализации структур данных.

Основным правилом при записи агрегатов является то, что *каждый
компонент* массива или записи должен быть указан, даже компоненты,
которые имеют значение по умолчанию.

Это означает, что следующий код неверен:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Incorrect_Aggregate
    :class: ada-expect-compile-error

    package Incorrect is
       type Point is record
          X, Y : Integer := 0;
       end record;

       Origin : Point := (X => 0);
    end Incorrect;

Существует несколько сокращений, которые можно использовать, чтобы
сделать представление более удобным:

-  Чтобы задать значение по умолчанию для компоненты, можно использовать
   нотацию :ada:`<>`.

-  Символ :ada:`|` можно использовать для присвоения нескольким компонентам
   одинакового значения.

-  Вы можете использовать :ada:`others` вариант для ссылки на все компоненты,
   которые еще не были указаны, при условии, что все эти поля имеют
   одинаковый тип.

-  Можно использовать нотацию диапазона :ada:`..` чтобы указать непрерывную
   последовательность индексов в массиве.

Однако следует отметить, что, как только вы использовали именованное
сопоставление, все последующие компоненты также должны быть указаны с
помощью именованного сопоставления.

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Points

    package Points is
       type Point is record
          X, Y : Integer := 0;
       end record;

       type Point_Array is
         array (Positive range <>) of Point;

       --  use the default values
       Origin   : Point := (X | Y => <>);

       --  likewise, use the defaults
       Origin_2 : Point := (others => <>);

       Points_1 : Point_Array := ((1, 2), (3, 4));
       Points_2 : Point_Array := (1       => (1, 2),
                                  2       => (3, 4),
                                  3 .. 20 => <>);
    end Points;

Cовмещение и квалифицированные выражения
----------------------------------------

В Ада есть общая концепция совмещения имен, которую мы видели ранее в
разделе о :ref:`перечислимых типах <Intro_Ada_Ru_EnumTypes>`.

Давайте возьмем простой пример: в Аде возможно иметь функции с
одинаковым именем, но разными типами для их параметров.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Overloading
    :class: ada-syntax-only

    package Pkg is
       function F (A : Integer) return Integer;
       function F (A : Character) return Integer;
    end Pkg;

Это распространенное понятие в языках программирования, называемое
совмещением, или `перегрузкой имен
<https://ru.m.wikipedia.org/wiki/Перегрузка_процедур_и_функций>`_.

Одной из особенностей совмещения имен в Аде является возможность
разрешить неоднозначности на основе типа возвращаемого функцией.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Overloading
    :class: ada-syntax-only

    package Pkg is
       type SSID is new Integer;

       function Convert (Self : SSID) return Integer;
       function Convert (Self : SSID) return String;
    end Pkg;

    with Ada.Text_IO; use Ada.Text_IO;
    with Pkg;         use Pkg;

    procedure Main is
       S : String := Convert (123_145_299);
       --            ^ Valid, will choose the
       --              proper Convert
    begin
       Put_Line (S);
    end Main;

.. attention::
    Заметим, что разрешение совмещения на основе типа в Аде работает как для
    функций, так и для литералов перечисления - вот почему у вас
    может быть несколько литералов перечисления с одинаковым именем.
    Семантически литерал перечисления рассматривается как функция, не
    имеющая параметров.

Однако иногда возникает двусмысленность из-за которой невозможно определить,
к какому объявлению совмещенного имени относится данное употребление имени.
Именно здесь становится полезным квалифицированное выражение.

.. code:: ada no_button project=Courses.Intro_To_Ada.More_About_Types.Overloading_Error
    :class: ada-syntax-only, ada-expect-compile-error

    package Pkg is
       type SSID is new Integer;

       function Convert (Self : SSID) return Integer;
       function Convert (Self : SSID) return String;
       function Convert (Self : Integer) return String;
    end Pkg;

    with Ada.Text_IO; use Ada.Text_IO;
    with Pkg;         use Pkg;

    procedure Main is
       S : String := Convert (123_145_299);
       --            ^ Invalid, which convert
       --              should we call?

       S2 : String := Convert (SSID'(123_145_299));
       --                     ^ We specify that the
       --                       type of the expression
       --                       is SSID.

       --  We could also have declared a temporary

       I : SSID := 123_145_299;

       S3 : String := Convert (I);
    begin
       Put_Line (S);
    end Main;

Синтаксически квалифицированное выражение используется либо с
выражением в круглых скобках, либо с агрегатом:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Qual_Expr

    package Qual_Expr is
       type Point is record
          A, B : Integer;
       end record;

       P : Point := Point'(12, 15);

       A : Integer := Integer'(12);
    end Qual_Expr;

Это иллюстрирует, что квалифицированные выражения являются удобным (а
иногда и необходимым) способом явно обозначить тип выражения
и помочь, как компилятору, так и для другим программистам разобраться
в коде.

.. attention::
    Хотя преобразования типов и квалифицированные выражения
    выглядят и ощущаются похожими это *не* одно и тоже.

    Квалифицированное выражение указывает точный тип, в котором следует
    трактовать выражение, в то время как преобразование типа
    попытается преобразовать значение и выдаст ошибку во время
    выполнения, если исходное значение не может быть преобразовано.

    Обратите внимание, что вы можете использовать квалифицированное
    выражение для преобразования из одного подтипа в другой, с
    исключением, возникающим при нарушении ограничения.

    .. code-block:: ada

        X : Integer := Natural'(1);

Символьные типы
---------------

Как отмечалось ранее, каждый перечислимый тип отличается и несовместим
с любым другим перечислимым типом. Однако ранее мы не упоминали, что
символьные литералы разрешены в качестве литералов перечисления. Это
означает, что в дополнение к стандартным символьным типам пользователь
может определить свои символьные типы:

.. code:: ada compile_button project=Courses.Intro_To_Ada.More_About_Types.Character_Example
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Character_Example is
       type My_Char is ('a', 'b', 'c');
       --  Our custom character type, an
       --  enumeration type with 3 valid values.

       C : Character;
       --  ^ Built-in character type
       --    (it's an enumeration type)

       M : My_Char;
    begin
       C := '?';
       --   ^ Character literal
       --     (enumeration literal)

       M := 'a';

       C := 65;
       --   ^ Invalid: 65 is not a
       --     Character value

       C := Character'Val (65);
       --  Assign the character at
       --  position 65 in the
       --  enumeration (which is 'A')

       M := C;
       --   ^ Invalid: C is of type Character,
       --     and M is a My_Char

       M := 'd';
       --   ^ Invalid: 'd' is not a valid
       --     literal for type My_Char
    end Character_Example;


