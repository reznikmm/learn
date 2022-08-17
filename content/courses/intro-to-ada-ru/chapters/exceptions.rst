Исключения
==========

.. include:: ../../global.txt

Ада использует исключения для обработки ошибок. В отличие от многих
других языков, в Аде принято говорить о *возбуждении*, а не о *выбрасывании*
исключений и их *обработке*, а не *перехвате*.

Объявление исключения
---------------------

Исключения в Аде - это не типы, а объекты, что может показаться вам
необычным, если вы привыкли к тому, как работают исключения в
Java или Python. Вот как вы объявляете исключение:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Exceptions.Show_Exception

    package Exceptions is
        My_Except : exception;
        --  Like an object. *NOT* a type !
    end Exceptions;

Несмотря на то, что они являются объектами, каждый объявленный объект
исключения вы используете как «класс» или «семейство» исключений.
Ада не требует, чтобы подпрограмма при объявлении указывала каждое
исключение, которое может быть возбуждено.

Возбуждение исключения
----------------------

Чтобы возбудить исключение нашего только что объявленного класса
исключения, сделайте следующее:

.. code:: ada run_button project=Courses.Intro_To_Ada.Exceptions.Show_Exception
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

Далее мы рассмотрим, как обрабатывать исключения, которые были возбуждены
нами или библиотеками, которые мы вызываем. Изящная вещь в Аде
заключается в том, что вы можете добавить обработчик исключений в
любой блок операторов следующим образом:

.. code:: ada run_button project=Courses.Intro_To_Ada.Exceptions.Show_Exception_Handling
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

В приведенном выше примере мы используем функцию :ada:`Exception_Message`
из пакета :ada:`Ada.Exceptions`. Эта
функция возвращает сообщение, связанное с исключением, в виде строки.

Вам не нужно вводить новый блок только чтобы обработать исключения: вы можете
добавить его в блок операторов вашей текущей подпрограммы:

.. code:: ada run_button project=Courses.Intro_To_Ada.Exceptions.Show_Exception_Message

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
    быть осторожным: исключения, созданные в разделе описаний, не
    перехватываются обработчиками этого блока. Так, например, в следующем
    коде исключение не будет поймано.

    .. code:: ada run_button project=Courses.Intro_To_Ada.Exceptions.Be_Careful
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

Ада имеет очень небольшое количество предопределенных исключений:

-  :ada:`Constraint_Error` является основным, с которым вы можете столкнуться.
   Возбуждение исключения Constraint_Error происходит:

   -  Когда происходит выход за границы массива или, в общем, любое нарушение
      ограничений
   -  В случае переполнения
   -  В случае обращения по пустой ссылке
   -  В случае деления на 0

-  :ada:`Program_Error` тоже может встетиться, но, вероятно, реже.
   Возбуждение исключения возникает в более сложных ситуациях, таких как
   проблемы с порядком предвыполнения и некоторые случаи обнаружения
   ошибочного выполнения.

-  :ada:`Storage_Error` произойдет из-за проблем с памятью, таких как:

   -  Недостаточно памяти (при распределении)
   -  Недостаточно стека

-  :ada:`Tasking_Error` сигнализирует об ошибках, связанных с задачами,
   такими как любая ошибка, возникающая во время активации задачи.

Не следует повторно использовать предопределенные исключения. Если вы
будете так делать, то при возбужденнии одного из них не будет ясно, связано
ли оно с работой встроенной языковой операции или нет.
