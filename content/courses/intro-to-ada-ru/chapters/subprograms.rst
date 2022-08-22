Подпрограммы
============

.. _Subprograms:

.. include:: ../../global.txt


Подпрограммы
------------

До сих пор мы использовали процедуры, в основном, чтобы расположить там
кода для исполнения. Процедуры являются одним из видов *подпрограмм*.

В Аде есть два вида подпрограмм: *функции* и *процедуры*. Различие между
ними заключается в том, что функция возвращает значение, а процедура -
нет.

В этом примере показано объявление и определение функции:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Subprograms.Increment

    function Increment (I : Integer) return Integer;
    --  We declare (but don't define) a function with
    --  one parameter, returning an integer value

    function Increment (I : Integer) return Integer is
       --  We define the Increment function
    begin
        return I + 1;
    end Increment;

Подпрограммы в Аде, конечно, могут иметь параметры. Одно важное замечание
касающееся синтаксиса
заключается в том, что подпрограмма, у которой нет
параметров, вообще не имеет раздела параметров, например:

.. code-block:: ada

    procedure Proc;

    function Func return Integer;

Вот еще один вариант предыдущего примера:

.. code:: ada no_button project=Courses.Intro_To_Ada.Subprograms.Increment_By
    :class: ada-syntax-only

    function Increment_By
      (I    : Integer := 0;
       Incr : Integer := 1) return Integer;
    --                ^ Default value for parameters

В этом примере мы видим, что параметры могут иметь значения по
умолчанию. При вызове подпрограммы вы можете опустить параметры, если
они имеют значение по умолчанию. В отличие от C/C++, вызов
подпрограммы без параметров не использует скобки.

Вот реализация функции, описанной выше:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Subprograms.Increment_By

    function Increment_By
      (I    : Integer := 0;
       Incr : Integer := 1) return Integer is
    begin
       return I + Incr;
    end Increment_By;

Вызовы подпрограмм
~~~~~~~~~~~~~~~~~~

Далее мы можем вызвать нашу подпрограмму следующим образом:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Increment_By
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;
    with Increment_By;

    procedure Show_Increment is
       A, B, C : Integer;
    begin
       C := Increment_By;
       --              ^ Parameterless call,
       --                value of I is 0
       --                and Incr is 1

       Put_Line ("Using defaults for Increment_By is "
                 & Integer'Image (C));

       A := 10;
       B := 3;
       C := Increment_By (A, B);
       --                 ^ Regular parameter passing

       Put_Line ("Increment of "
                 & Integer'Image (A)
                 & " with "
                 & Integer'Image (B)
                 & " is "
                 & Integer'Image (C));

       A := 20;
       B := 5;
       C := Increment_By (I    => A,
                          Incr => B);
        --                ^ Named parameter passing

       Put_Line ("Increment of "
                 & Integer'Image (A)
                 & " with "
                 & Integer'Image (B)
                 & " is "
                 & Integer'Image (C));
    end Show_Increment;

Ада позволяет вам выполнять указывать имена параметров при
передачи их во время вызова, независимо от того, есть ли значения
по умолчанию или нет. Но есть несколько правил:

-  Позиционные параметры должны идти первыми.

-  Позиционный параметр не может следовать за именованным параметром.

Как правило, пользователи используют именованные параметры во
время вызова, если соответствующий параметр функции имеет значение по
умолчанию. Однако также вполне приветствуется использовать вызов с
именованием каждого параметра, если это делает код более понятным.

Вложенные подпрограммы
~~~~~~~~~~~~~~~~~~~~~~

Как кратко упоминалось ранее, Ада позволяет вам объявлять одну
подпрограмму внутри другой.

Это полезно по двум причинам:

-  Это позволяет вам получить более понятную программу.
   Если вам нужна подпрограмма только как «помощник» для другой
   подпрограммы, то принцип локализации указывает, что
   подпрограмма-помощник должна быть объявлена вложенной.

-  Это облегчает вам доступ к данным объемлющей подпрограммы и сохранить
   при этом контроль, потому что вложенные подпрограммы имеют доступ к
   параметрам, а также к любым локальным переменным, объявленным во внешней
   области.

Используя предыдущий пример, можно переместить часть кода
(вызов :ada:`Put_Line`) в отдельную процедуру и избежать
дублирования. Вот укороченная версия с вложенной
процедурой :ada:`Display_Result`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Increment_By
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;
    with Increment_By;

    procedure Show_Increment is
       A, B, C : Integer;

       procedure Display_Result is
       begin
          Put_Line ("Increment of "
                    & Integer'Image (A)
                    & " with "
                    & Integer'Image (B)
                    & " is "
                    & Integer'Image (C));
       end Display_Result;

    begin
       A := 10;
       B := 3;
       C := Increment_By (A, B);
       Display_Result;
    end Show_Increment;

Вызов функций
~~~~~~~~~~~~~

Важной особенностью вызовов функций в Аде является то, что
возвращаемое значение при вызове нельзя игнорировать; то есть вызов
функции не может использоваться как оператор.

Если вы хотите вызвать функцию и вам не нужен ее результат, вам все
равно нужно будет явно сохранить его в локальной переменной.

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Quadruple
    :class: ada-expect-compile-error

    function Quadruple (I : Integer) return Integer is
        function Double (I : Integer) return Integer is
        begin
           return I * 2;
        end Double;

       Res : Integer := Double (Double (I));
       --               ^ Calling the Double
       --                 function
    begin
       Double (I);
       --  ERROR: cannot use call to function
       --         "Double" as a statement

       return Res;
    end Quadruple;

.. admonition:: В наборе инструментов GNAT

    В GNAT, когда все предупреждения активированы, становится еще сложнее
    игнорировать результат функции, потому что неиспользуемые переменные
    будут выявлены. Например, этот код будет недействительным:

    .. code-block:: ada

        function Read_Int
           (Stream :     Network_Stream;
            Result : out Integer) return Boolean;

        procedure Main is
            Stream : Network_Stream := Get_Stream;
            My_Int : Integer;

            -- Warning: in the line below, B is
            --          never read.
            B : Boolean := Read_Int (Stream, My_Int);
        begin
           null;
        end Main;

    Затем у вас есть два решения, чтобы отключить это предупреждение:

    - Либо аннотировать переменную с помощью pragma Unreferenced, таким
      образом:

    .. code-block:: ada

        B : Boolean := Read_Int (Stream, My_Int);
        pragma Unreferenced (B);

    - Или дайть переменной имя, которое содержит любую из строк: :ada:`discard`
      :ada:`dummy` :ada:`ignore` :ada:`junk` :ada:`unused` (без учета регистра)

Виды параметров
---------------

До сих пор мы видели, что Ада - это язык, ориентированный на
безопасность. Существует много механизмов реализации этого принципа,
но два важных момента заключаются в следующем:

-  Ада позволяет пользователю как можно более точно указать ожидаемое
   поведение программы, чтобы компилятор мог предупреждать или отклонять при
   обнаружении несоответствия.

-  Ада предоставляет множество методов для достижения общности и гибкости
   указателей и динамического управления памятью, но без недостатков
   последнего (таких как утечка памяти и висячие ссылки).

Виды параметров - это возможность, которая помогает воплотить эти
два момента на практике. Параметр подпрограммы может быть
одного из следующих видов:

+---------------+--------------------------------------------------+
| :ada:`in`     | Параметр может быть только считан, но не записан |
+---------------+--------------------------------------------------+
| :ada:`out`    | Параметр можно записать, а затем прочитать       |
+---------------+--------------------------------------------------+
| :ada:`in out` | Параметр может быть, как считан, так и записан   |
+---------------+--------------------------------------------------+

По умолчанию вид параметра будет :ada:`in`; до сих пор большинство
примеров использовали параметры вида :ada:`in`.

.. admonition:: Исторически

    Функции и процедуры изначально были более разными по философии. До Ада
    2012 функции могли принимать только «входящие» (:ada:`in`) параметры.

Вызов процедуры
---------------

Параметры in
~~~~~~~~~~~~

Первый вид параметра - это тот, который мы неявно использовали
до сих пор. Параметры этого вида нельзя изменить, поэтому следующая программа
вызовет ошибку:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Swap
    :class: ada-expect-compile-error

    procedure Swap (A, B : Integer) is
       Tmp : Integer;
    begin
       Tmp := A;

       --  Error: assignment to "in" mode
       --         parameter not allowed
       A := B;

       --  Error: assignment to "in" mode
       --         parameter not allowed
       B := Tmp;
    end Swap;

Тот факт, что это вид используется по умолчанию, сам по себе очень важен. Это
означает, что параметр не будет изменен, если вы явно не укажете
ему другой вид, для которого разрешено изменение.

Параметры in out
~~~~~~~~~~~~~~~~

Для исправления кода, приведенного выше, можно использовать параметр :ada:`in out`
.

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.In_Out_Params
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure In_Out_Params is
       procedure Swap (A, B : in out Integer) is
          Tmp : Integer;
       begin
          Tmp := A;
          A   := B;
          B   := Tmp;
       end Swap;

       A : Integer := 12;
       B : Integer := 44;
    begin
        Swap (A, B);

        --  Prints 44
        Put_Line (Integer'Image (A));
    end In_Out_Params;

Параметр :ada:`in out` обеспечивает доступ для чтения и записи к объекту,
переданному в качестве параметра, поэтому в приведенном выше примере
видно, что значение :ada:`A` изменяется после вызова функции :ada:`Swap`.

.. attention::

    В то время как параметры :ada:`in out` немного похожи на ссылки в C++ или
    обычные параметры в Java, которые передаются по ссылке, стандарт языка
    Ада не требует передачи параметров :ada:`in out` "по ссылке", за исключением
    определенных категорий типов, как будет объяснено позже.

    В общем, лучше думать о видах параметров как о более высоком уровне, чем о
    семантике «по значению» или «по ссылке». Для компилятора это означает, что
    массив, передаваемый в качестве параметра :ada:`in`, может передаваться по
    ссылке, поскольку это более эффективено (что ничего не меняет для
    пользователя, поскольку параметр не может быть назначен). Однако
    параметр дискретного типа всегда будет передаваться копией, независимо
    от его вида (ведь так более эффективено на большинстве архитектур).

Параметры out
~~~~~~~~~~~~~

Вид «:ada:`out`» применяется, когда подпрограмме необходимо выполнить запись в
параметр, который может быть не инициализирован в момент вызова.
Чтение значения выходного параметра разрешено, но оно должно
выполняться только после того, как подпрограмма присвоила значение
параметру. Параметры :ada:`out` немного похожи на возвращаемые значения
функций. Когда подпрограмма возвращается, фактический параметр
(переменная) будет иметь значение параметра в точке возврата.

.. admonition:: На других языках

    Ада не имеет конструкции кортежа и не позволяет возвращать несколько
    значений из подпрограммы (за исключением объявления полноценного типа
    записи). Следовательно, способ вернуть несколько значений из
    подпрограммы состоит в использовании параметров out.

Например, процедура считывания целых чисел из сети может иметь одну из
следующих спецификаций:

.. code-block:: ada

    procedure Read_Int
       (Stream  :     Network_Stream;
        Success : out Boolean;
        Result  : out Integer);

    function Read_Int
       (Stream :     Network_Stream;
        Result : out Integer) return Boolean;

При чтении переменной :ada:`out` до записью в нее в идеале должна
возникать ошибка, но, если бы было введено такое правило, то это бы привело
либо к неэффективным проверкам во время выполнения, либо к очень сложным
правилам во время компиляции. Таким образом, с точки зрения пользователя
параметр :ada:`out` действует как неинициализированная в момент
вызова подпрограммы переменная.

.. admonition:: В наборе инструментов GNAT

    GNAT обнаружит простые случаи неправильного использования параметров
    :ada:`out`. Например, компилятор выдаст предупреждение для следующей
    программы:

    .. code:: ada compile_button project=Courses.Intro_To_Ada.Subprograms.Out_Params

        procedure Outp is
           procedure Foo (A : out Integer) is
              B : Integer := A;
              --             ^ Warning on reference
              --               to uninitialized A
           begin
              A := B;
           end Foo;
        begin
           null;
        end Outp;

Предварительное объявление подпрограмм
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Как мы видели ранее, подпрограмма может быть объявлена без полного
определения. Это возможно в целом и может быть полезно, если вам
нужно, чтобы подпрограммы были взаимно рекурсивными, как в примере
ниже:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Mutually_Recursive_Subprograms
    :class: ada-run

    procedure Mutually_Recursive_Subprograms is
        procedure Compute_A (V : Natural);
        --  Forward declaration of Compute_A

        procedure Compute_B (V : Natural) is
        begin
           if V > 5 then
              Compute_A (V - 1);
              --  Call to Compute_A
           end if;
        end Compute_B;

        procedure Compute_A (V : Natural) is
        begin
           if V > 2 then
              Compute_B (V - 1);
              --  Call to Compute_B
           end if;
        end Compute_A;
    begin
       Compute_A (15);
    end Mutually_Recursive_Subprograms;

.. _Subprogram_Renaming:

Переименование
--------------

Подпрограммы можно переименовать, используя ключевое слово :ada:`renames` и объявив
новое имя для подпрограммы:

.. code-block:: ada

    procedure New_Proc renames Original_Proc;

Это может быть полезно, например, для улучшения читаемости вашей
программы, когда вы используете код из внешних источников, который
нельзя изменить в вашей системе. Давайте посмотрим на пример:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Subprograms.Proc_Renaming

    procedure A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed
      (A_Message : String);

    with Ada.Text_IO; use Ada.Text_IO;

    procedure A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed
      (A_Message : String) is
    begin
       Put_Line (A_Message);
    end A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed;

Как следует из названия процедуры, мы
не можем изменить его. Однако мы можем переименовать процедуру во
что-то вроде :ada:`Show` в нашем тестовом приложении и использовать это более
короткое имя. Обратите внимание, что мы также должны объявить все
параметры исходной подпрограммы, но мы можем именовать их по другому при
объявлении. Например:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Proc_Renaming

    with A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed;

    procedure Show_Renaming is

       procedure Show (S : String) renames
         A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed;

    begin
       Show ("Hello World!");
    end Show_Renaming;

Обратите внимание, что исходное имя
(:ada:`A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed`)
по-прежнему остается доступно после объявления процедуры :ada:`Show`.

Можно также переименовать подпрограммы из стандартной библиотеки.
Например, можно переименовать :ada:`Integer'Image` в :ada:`Img`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Subprograms.Integer_Image_Renaming

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Image_Renaming is

       function Img (I : Integer) return String
         renames Integer'Image;

    begin
       Put_Line (Img (2));
       Put_Line (Img (3));
    end Show_Image_Renaming;

Переименование также позволяет вводить выражения по умолчанию, которые
не были указаны в исходном объявлении. Например, можно задать
:ada:`"Hello World!"` в качестве значения по умолчанию для параметра
:ada:`String` процедуры :ada:`Show`:

.. code-block:: ada

    with A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed;

    procedure Show_Renaming_Defaults is

       procedure Show (S : String := "Hello World!")
         renames
           A_Procedure_With_Very_Long_Name_That_Cannot_Be_Changed;

    begin
       Show;
    end Show_Renaming_Defaults;


