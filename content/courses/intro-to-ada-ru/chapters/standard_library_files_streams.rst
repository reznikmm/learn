Стандартная библиотека: Файлы и потоки
======================================

.. include:: ../../global.txt

Ada предлагает различные подходы для ввода/вывода файлов (I/O):

-  *Text I/O* Текстовый ввод-вывод, поддерживающий файловый ввод-вывод в текстовом
   формате, включая отображение информации на консоли.

-  *Sequential I/O* Последовательный ввод-вывод, поддерживающий ввод-вывод файлов в
   двоичном формате, написанный последовательным образом для
   определенного типа данных.

-  *Direct I/O* Прямой ввод-вывод, поддерживающий ввод-вывод файлов в двоичном
   формате для определенного типа данных, но также поддерживающий доступ
   к любой позиции файла.

-  *Stream I/O* Потоковый ввод-вывод, поддерживающий ввод-вывод информации для
   нескольких типов данных, включая объекты неограниченных типов, с
   использованием файлов в двоичном формате.

В этой таблице представлено краткое описание функций, которые мы
только что видели:

+----------------+------------+--------------+-----------------+
| Опция ввода-   | Формат     | Произвольный | Типы данных     |
| вывода файлов  |            | доступ       |                 |
+================+============+==============+=================+
| Text I/O       | текст      |              | строковый тип   |
+----------------+------------+--------------+-----------------+
| Sequential I/O | двоичный   |              | одиночный тип   |
+----------------+------------+--------------+-----------------+
| Direct I/O     | двоичный   | X            | одиночный тип   |
+----------------+------------+--------------+-----------------+
| Stream I/O     | двоичный   | X            | несколько типов |
+----------------+------------+--------------+-----------------+

В следующих разделах мы подробно обсудим эти подходы ввода-вывода.

Ввод/вывод текста
-----------------

В большинстве частей этого курса мы использовали процедуру :ada:`Put_Line` для
отображения информации на консоли. Однако эта процедура также
принимает параметр :ada:`File_Type`. Например, вы можете выбрать между стандартным
выводом (Standard_Output) и выводом стандартной ошибкой
(Standard_Error), явно задав этот параметр:

.. code-block:: ada

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Std_Text_Out is
    begin
       Put_Line (Standard_Output, "Hello World #1");
       Put_Line (Standard_Error,  "Hello World #2");
    end Show_Std_Text_Out;

Вы также можете использовать этот параметр для записи информации в
любой текстовый файл. Чтобы создать новый файл для записи, используйте
процедуру :ada:`Create`, которая инициализирует элемент :ada:`File_Type`, который впоследствии
можно передать в :ada:`Put_Line` (вместо, например, :ada:`Standard_Output`). После того, как вы закончите
ввод информации, вы можете закрыть файл, вызвав процедуру :ada:`Close`.

Вы используете аналогичный метод для чтения информации из текстового
файла. Однако при открытии файла вы должны указать, что это входной
файл (:ada:`In_File`), а не выходной файл. Кроме того, вместо вызова процедуры :ada:`Put_Line` вы
вызываете функцию :ada:`Get_Line` для чтения информации из файла.

Давайте посмотрим на пример, который записывает информацию в новый
текстовый файл, а затем считывает ее обратно из того же файла:

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Simple_Text_File_IO is
       F         : File_Type;
       File_Name : constant String := "simple.txt";
    begin
       Create (F, Out_File, File_Name);
       Put_Line (F, "Hello World #1");
       Put_Line (F, "Hello World #2");
       Put_Line (F, "Hello World #3");
       Close (F);

       Open (F, In_File, File_Name);
       while not End_Of_File (F) loop
          Put_Line (Get_Line (F));
       end loop;
       Close (F);
    end Show_Simple_Text_File_IO;

В дополнение к процедурам :ada:`Create` и :ada:`Close` стандартная библиотека также включает
процедуру :ada:`Reset`, которая, как следует из названия, сбрасывает (стирает) всю
информацию из файла. Например:

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Text_File_Reset is
       F         : File_Type;
       File_Name : constant String := "simple.txt";
    begin
       Create (F, Out_File, File_Name);
       Put_Line (F, "Hello World #1");
       Reset (F);
       Put_Line (F, "Hello World #2");
       Close (F);

       Open (F, In_File, File_Name);
       while not End_Of_File (F) loop
          Put_Line (Get_Line (F));
       end loop;
       Close (F);
    end Show_Text_File_Reset;

Запустив эту программу, мы замечаем, что, хотя мы записали первую
строку (:ada:`Hello World #1`) в файл, она была удалена из-за вызова процедуры сброса :ada:`Reset`.

В дополнение к открытию файла для чтения или записи, вы также можете
открыть существующий файл и добавить к нему. Сделайте это, вызвав
процедуру :ada:`Open` с параметром :ada:`Append_File`.

При вызове процедуры открытия :ada:`Open` возникает исключение, если указанный
файл не найден. Поэтому вы должны обрабатывать исключения в этом
контексте. В следующем примере удаляется файл, а затем предпринимается
попытка открыть тот же файл для чтения:

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Text_File_Input_Except is
       F         : File_Type;
       File_Name : constant String := "simple.txt";
    begin
       --  Open output file and delete it
       Create (F, Out_File, File_Name);
       Delete (F);

       --  Try to open deleted file
       Open (F, In_File, File_Name);
       Close (F);
    exception
       when Name_Error =>
          Put_Line ("File does not exist");
       when others =>
          Put_Line ("Error while processing input file");
    end Show_Text_File_Input_Except;

В этом примере файл создается вызовом :ada:`Create`, а затем удаляется вызовом :ada:`Delete`.
После вызова функции :ada:`Delete` мы больше не можем использовать элемент :ada:`File_Type`. После
удаления файла мы пытаемся открыть несуществующий файл, что вызывает :ada:`Name_Error`
исключение.

Последовательный ввод-вывод
---------------------------

В предыдущем разделе представлена подробная информация о текстовом
файле ввода/вывода. Здесь мы обсудим выполнение файловых операций
ввода-вывода в двоичном формате. Первый пакет, который мы рассмотрим ‑
это :ada:`Ada.Sequential_IO` пакет. Поскольку этот пакет является универсальным, необходимо
создать его экземпляр для типа данных, который требуется использовать
для ввода-вывода файла. После этого можно использовать те же
процедуры, что и в предыдущем разделе: Создание, Открытие, Закрытие,
Сброс и Удаление (:ada:`Create`, :ada:`Open`, :ada:`Close`, :ada:`Reset` и :ada:`Delete`). Однако вместо вызова процедур :ada:`Get_Line` и :ada:`Put_Line`
следует вызвать процедуры :ada:`Read` и :ada:`Write` (Чтения и Записи).

В следующем примере создается экземпляр пакета :ada:`Ada.Sequential_IO` для типов с плавающей
запятой:

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO;
    with Ada.Sequential_IO;

    procedure Show_Seq_Float_IO is
       package Float_IO is
         new Ada.Sequential_IO (Float);
       use Float_IO;

       F         : Float_IO.File_Type;
       File_Name : constant String := "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       Write (F,  1.5);
       Write (F,  2.4);
       Write (F,  6.7);
       Close (F);

       declare
          Value : Float;
       begin
          Open (F, In_File, File_Name);
          while not End_Of_File (F) loop
             Read (F, Value);
             Ada.Text_IO.Put_Line (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Seq_Float_IO;

Мы используем один и тот же подход для чтения и записи сложной
структурной информации. В следующем примере используется тип запись
(record), включающая логическое значение типа Boolean и значение с
плавающей запятой типа Float:

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO;
    with Ada.Sequential_IO;

    procedure Show_Seq_Rec_IO is
       type Num_Info is record
          Valid : Boolean := False;
          Value : Float;
       end record;

       procedure Put_Line (N : Num_Info) is
       begin
          if N.Valid then
             Ada.Text_IO.Put_Line ("(ok,     "
                                   & Float'Image (N.Value) & ")");
          else
             Ada.Text_IO.Put_Line ("(not ok,  -----------)");
          end if;
       end Put_Line;

       package Num_Info_IO is new Ada.Sequential_IO (Num_Info);
       use Num_Info_IO;

       F         : Num_Info_IO.File_Type;
       File_Name : constant String := "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       Write (F,  (True,  1.5));
       Write (F,  (False, 2.4));
       Write (F,  (True,  6.7));
       Close (F);

       declare
          Value : Num_Info;
       begin
          Open (F, In_File, File_Name);
          while not End_Of_File (F) loop
             Read (F, Value);
             Put_Line (Value);
          end loop;
          Close (F);
       end;
    end Show_Seq_Rec_IO;

Как показывает пример, мы можем использовать тот же подход, который мы
использовали для типов с плавающей запятой, для выполнения файлового
ввода-вывода для этой записи. После того, как мы создадим экземпляр
пакета :ada:`Ada.Sequential_IO` для типа записи, операции ввода-вывода файлов будут
выполняться таким же образом.

Прямой ввод / вывод
-------------------

Прямой ввод-вывод доступен в пакете :ada:`Ada.Direct_IO`. Этот механизм похож на только
что представленный подход последовательного ввода-вывода, но позволяет
нам получить доступ к любой позиции в файле. Создание пакета и
большинство операций очень похожи на последовательный ввод-вывод.
Чтобы переписать приложение :ada:`Show_Seq_Float_IO`, представленное в предыдущем разделе, для
использования пакета :ada:`Ada.Direct_IO`, нам просто нужно заменить экземпляры пакета :ada:`Ada.Sequential_IO` на
пакет :ada:`Ada.Direct_IO`. Это новый исходный код:

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO;
    with Ada.Direct_IO;

    procedure Show_Dir_Float_IO is
       package Float_IO is new Ada.Direct_IO (Float);
       use Float_IO;

       F         : Float_IO.File_Type;
       File_Name : constant String := "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       Write (F,  1.5);
       Write (F,  2.4);
       Write (F,  6.7);
       Close (F);

       declare
          Value : Float;
       begin
          Open (F, In_File, File_Name);
          while not End_Of_File (F) loop
             Read (F, Value);
             Ada.Text_IO.Put_Line (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Dir_Float_IO;

В отличие от последовательного ввода-вывода, прямой ввод-вывод
позволяет получить доступ к любой позиции в файле. Однако он не
предлагает возможность добавлять информацию в файл. Вместо этого он
предоставляет режим :ada:`Inout_File`, позволяющий читать и записывать в файл через
один и тот же элемент :ada:`File_Type`.

Чтобы получить доступ к любой позиции в файле, вызовите процедуру :ada:`Set_Index`,
чтобы установить новую позицию / индекс. Вы можете использовать
функцию :ada:`Index` для получения текущего индекса. Посмотрим на пример:

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO;
    with Ada.Direct_IO;

    procedure Show_Dir_Float_In_Out_File is
       package Float_IO is new Ada.Direct_IO (Float);
       use Float_IO;

       F         : Float_IO.File_Type;
       File_Name : constant String := "float_file.bin";
    begin
       --  Open file for input / output
       Create (F, Inout_File, File_Name);
       Write (F,  1.5);
       Write (F,  2.4);
       Write (F,  6.7);

       --  Set index to previous position and overwrite value
       Set_Index (F, Index (F) - 1);
       Write (F,  7.7);

       declare
          Value : Float;
       begin
          --  Set index to start of file
          Set_Index (F, 1);

          while not End_Of_File (F) loop
             Read (F, Value);
             Ada.Text_IO.Put_Line (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Dir_Float_In_Out_File;

Запустив этот пример, мы видим, что файл содержит версию ``7.7``, а не
предыдущую версию ``6.7``, которую мы написали. Мы перезаписали значение,
изменив индекс на предыдущую позицию перед выполнением следующей
записи.

В этом примере мы использовали режим :ada:`Inout_File`. Используя этот режим, мы просто
вернули индекс в исходное положение перед чтением из файла (:ada:`Set_Index (F, 1)`) вместо
того, чтобы закрывать файл и повторно открывать его для чтения.

Потоковый ввод-вывод
--------------------

Все предыдущие подходы к файловому вводу-выводу в двоичном формате
(последовательный и прямой ввод-вывод) специфичны для одного типа
данных (того, с помощью которого мы их создаем). Вы можете
использовать эти подходы для записи объектов одного типа данных,
которые могут быть массивом или записью (потенциально со многими
полями), но если вам нужно создать и обработать файлы, которые
включают разные типы данных, или любые объекты неограниченного типа,
этих подходов недостаточно. Вместо этого вы должны использовать
потоковый ввод-вывод.

Потоковый ввод-вывод имеет некоторые общие черты с предыдущими
подходами. Мы по-прежнему используем процедуры :ada:`Create`, :ada:`Open` и :ada:`Close`. Однако вместо
прямого доступа к файлу через элемент :ada:`File_Type` вы используете элемент :ada:`Stream_Access`. Для
чтения и записи информации вы используете атрибуты :ada:`'Read` или :ada:`'Write` для типов
данных, которые вы читаете или пишете.

Давайте посмотрим на версию процедуры :ada:`Show_Dir_Float_IO` из предыдущего раздела.
Процедура использует потоковый ввод-вывод вместо прямого ввода-вывода:

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO;
    with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

    procedure Show_Float_Stream is
       F         : File_Type;
       S         : Stream_Access;
       File_Name : constant String := "float_file.bin";
    begin
       Create (F, Out_File, File_Name);
       S := Stream (F);

       Float'Write (S, 1.5);
       Float'Write (S, 2.4);
       Float'Write (S, 6.7);

       Close (F);

       declare
          Value : Float;
       begin
          Open (F, In_File, File_Name);
          S := Stream (F);

          while not End_Of_File (F) loop
             Float'Read (S, Value);
             Ada.Text_IO.Put_Line (Float'Image (Value));
          end loop;
          Close (F);
       end;
    end Show_Float_Stream;

После вызова :ada:`Create` мы получаем соответствующий элемент :ada:`Stream_Access`, вызывая функцию :ada:`Stream`.
Затем мы используем этот поток для записи информации в файл через
атрибут :ada:`'Write` типа :ada:`Float`. После закрытия файла и повторного открытия его для
чтения мы снова получаем соответствующий элемент :ada:`Stream_Access` и обрабатываем его
для чтения информации из файла через атрибут :ada:`'Read` типа :ada:`Float`.

Вы можете использовать потоки для создания и обработки файлов,
содержащих разные типы данных в одном файле. Вы также можете читать и
записывать неограниченные типы данных, такие как строки. Однако при
использовании неограниченных типов данных вы должны вызывать атрибуты :ada:`'Input`
и :ada:`'Output` неограниченного типа данных: эти атрибуты записывают информацию о
границах или дискриминантах в дополнение к фактическим данным объекта.

В следующем примере показан файловый ввод-вывод, который смешивает как
строки разной длины, так и значения с плавающей запятой:

.. code-block:: ada
    :class: ada-run

    with Ada.Text_IO;
    with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

    procedure Show_String_Stream is
       F         : File_Type;
       S         : Stream_Access;
       File_Name : constant String := "float_file.bin";

       procedure Output (S  : Stream_Access;
                         FV : Float;
                         SV : String) is
       begin
          String'Output (S, SV);
          Float'Output (S,  FV);
       end Output;

       procedure Input_Display (S : Stream_Access) is
          SV : String := String'Input (S);
          FV : Float  := Float'Input (S);
       begin
          Ada.Text_IO.Put_Line (Float'Image (FV)
                                & " --- " & SV);
       end Input_Display;

    begin
       Create (F, Out_File, File_Name);
       S := Stream (F);

       Output (S, 1.5, "Hi!!");
       Output (S, 2.4, "Hello world!");
       Output (S, 6.7, "Something longer here...");

       Close (F);

       Open (F, In_File, File_Name);
       S := Stream (F);

       while not End_Of_File (F) loop
          Input_Display (S);
       end loop;
       Close (F);

    end Show_String_Stream;

Когда вы используете потоковый ввод-вывод, в файл не записывается
никакая информация, указывающая тип данных, которые вы записали. Если
файл содержит данные разных типов, при чтении файла вы должны
ссылаться на типы в том же порядке, что и при его написании. В
противном случае полученная информация будет повреждена. К сожалению,
строгая типизация данных в этом случае вам не поможет. Написание
простых процедур для файлового ввода-вывода (как в приведенном выше
примере) может помочь обеспечить согласованность формата файла.

Как и прямой ввод-вывод, поддержка потокового ввода-вывода также
позволяет получить доступ к любому месту в файле. Однако при этом
нужно быть предельно осторожным, чтобы положение нового индекса
соответствовало ожидаемым типам данных.


