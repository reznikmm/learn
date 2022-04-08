Исключения
==========

.. include:: ../../global.txt

Ada использует исключения для обработки ошибок. В отличие от многих
других языков, Ada говорит о *возбуждении*, а не о *выбрасывании*
исключения и *обработке*, а не о *перехвате* исключения.

Объявление исключения
---------------------

Исключения Ada ‑ это не типы, а объекты, которые могут быть
специфическими для вас, если вы привыкли к тому, как Java или Python
поддерживают исключения. Вот как вы объявляете исключение:

.. code-block:: ada

    package Exceptions is
        My_Except : exception;
        --  Like an object. *NOT* a type !
    end Exceptions;

Несмотря на то, что они являются объектами, каждый объявленный объект
исключения будет использоваться как «тип» или «семейство» исключений.
Ada не требует, чтобы подпрограмма объявляла каждое возможное
исключение.

Возбуждение исключения
----------------------

Чтобы вызвать (возбудить) исключение нашего недавно объявленного вида
исключения, сделайте следующее:

.. code-block:: ada
    :class: ada-run-expect-failure

    with Exceptions; use Exceptions;

    procedure Main is
    begin
       raise My_Except;
       --  Execution of current control flow
       --  abandoned; an exception of kind
       --  "My_Except" will bubble up until it
       --  is caught.

       raise My_Except with "My exception message";
       --  Execution of current control flow
       --  abandoned; an exception of kind
       --  "My_Except" with associated string will
       --  bubble up until it is caught.
    end Main;

Обработка исключения
--------------------

Далее мы рассмотрим, как обрабатывать исключения, которые были созданы
нами или библиотеками, которые мы вызываем. Изящная вещь в Ada
заключается в том, что вы можете добавить обработчик исключений в
любой блок операторов следующим образом:

.. code-block:: ada
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Exceptions;  use Ada.Exceptions;

    procedure Open_File is
       File : File_Type;
    begin
       --  Block (sequence of statements)
       begin
          Open (File, In_File, "input.txt");
       exception
          when E : Name_Error =>
          --       ^ Exception to be handled
             Put ("Cannot open input file : ");
             Put_Line (Exception_Message (E));
             raise;
             --  Reraise current occurence
       end;
    end Open_File;

В приведенном выше примере мы используем функцию :ada:`Exception_Message` из пакета :ada:`Ada.Exceptions`. Эта
функция возвращает сообщение, связанное с исключением, в виде строки.

Вам не нужно вводить блок только для обработки исключения: вы можете
добавить его в блок инструкций вашей текущей подпрограммы:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Exceptions;  use Ada.Exceptions;

    procedure Open_File is
       File : File_Type;
    begin
       Open (File, In_File, "input.txt");
    --  Exception block can be added to any block
    exception
       when Name_Error =>
          Put ("Cannot open input file");
    end Open_File;

.. admonition:: Обратите внимание

    Обработчики исключений имеют важное ограничение, с которым вам нужно
    быть осторожным: исключения, созданные в декларативном разделе, не
    перехватываются обработчиками этого блока. Так, например, в следующем
    коде исключение не будет поймано.

    .. code-block:: ada
        :class: ada-run-expect-failure

        with Ada.Text_IO; use Ada.Text_IO;
        with Ada.Exceptions;  use Ada.Exceptions;

        procedure Be_Careful is
           function Dangerous return Integer is
           begin
              raise Constraint_Error;
              return 42;
           end Dangerous;

        begin
           declare
              A : Integer := Dangerous;
           begin
              Put_Line (Integer'Image (A));
           exception
              when Constraint_Error =>
                 Put_Line ("error!");
           end;
        end Be_Careful;

    Это также относится к блоку исключений верхнего уровня, который
    является частью текущей подпрограммы.

Предопределенные исключения
---------------------------

Ada имеет очень небольшое количество предопределенных исключений:

-  :ada:`Constraint_Error` является основным, который вы можете увидеть. Возбуждение исключения
   Constraint_Error происходит:

   -  Когда границы не совпадают или, в общем, любое нарушение ограничений.
   -  В случае переполнения
   -  В случае нулевых разыменований
   -  В случае деления на 0

-  :ada:`Program_Error` может появляться, но, вероятно, реже. Возбуждение исключения
   возникает в более сложных ситуациях, таких как проблемы с порядком
   разработки и некоторые случаи обнаруживаемого ошибочного выполнения.

-  :ada:`Storage_Error` произойдет из-за проблем с памятью, таких как:

   -  Недостаточно памяти (allocator)
   -  Недостаточно стека

-  :ada:`Tasking_Error` произойдет с ошибками, связанными с задачей, такими как любая ошибка,
   возникающая во время активации задачи.

Не следует повторно использовать предопределенные исключения. Если вы
это сделаете, то при появлении одного из них не будет очевидно, что
это связано с тем, что что-то пошло не так во встроенной языковой
операции.
