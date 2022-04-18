.. meta::
  :author: AdaCore

:prev_state: False
:next_state: False

.. include:: <isopub.txt>

.. only:: builder_html

    LEARN.ADACORE.COM на русском
    ===================

.. only:: builder_latex or builder_epub

    Изучая Аду
    ==========

.. only:: builder_html

    .. raw:: html

        <a href="https://github.com/reznikmm/learn"><i class="fab fa-github"></i> Править на GitHub</a><br><br>

    Что такое Ада и SPARK?
    ----------------------

    Ада |mdash| это современный язык программирования, который разработчики
    по всему миру используют для создания критически важных программных
    систем: от микроядер и небольших встроенных систем реального времени
    до крупномасштабных корпоративных приложений.

    SPARK |mdash| это подмножество языка Ада поддающееся формальному анализу,
    а также набор инструментов, проверяющий корректность программы с помощью
    математических методов.

    Попробовать Аду прямо сейчас:
    -----------------------------

    .. code:: ada run_button project=Introduction main=learn.adb

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Learn is

           subtype Alphabet is Character range 'A' .. 'Z';

        begin

           Put_Line ("Learning Ada from " & Alphabet'First & " to " & Alphabet'Last);

        end Learn;

    Просмотрите интерактивные курсы и лабораторные работы, перечисленные слева,
    чтобы узнать больше про Аду и SPARK.

    -------------

.. container:: content-blocks

    .. only:: builder_html

        .. toctree::
            :maxdepth: 4

            Подробнее <about>

    .. toctree::
        :maxdepth: 1
        :caption: Курсы

        Введение в язык Ада <courses/intro-to-ada-ru/index>

.. only:: builder_html

    Электронные книги
    -----------------

    Загрузите содержимое всего веб-сайта в виде электронной книги для чтения
    в автономном режиме. Вы можете выбрать следующие форматы: PDF, EPUB и
    MOBI (для устройств Kindle).

    .. container:: frontpage-ebooks

        .. container:: frontpage-ebooks-row

            .. container:: frontpage-ebook-and-buttons-block

                .. image:: images/page-1-of-learning-ada.jpeg
                    :alt: Learning Ada (e-book)
                    :width: 149pt

                .. raw:: html

                        <div class="frontpage-ebook-download">
                            <a class="ebook-download-button" href="pdf_books/learning-ada.pdf">
                                PDF
                            </a>

                            <a class="ebook-download-button" href="epub_books/learning-ada.epub">
                                EPUB
                            </a>

                            <a class="ebook-download-button" href="mobi_books/learning-ada.mobi">
                                MOBI
                            </a>
                        </div>

    Либо загрузите отдельные курсы и лабораторные работы в виде электронных книг:

    .. container:: frontpage-ebooks

        .. container:: frontpage-ebooks-row

            .. container:: frontpage-ebook-and-buttons-block

                .. image:: images/page-1-of-intro-to-ada.jpeg
                    :alt: Введение в язык Ада
                    :width: 149pt

                .. raw:: html

                        <div class="frontpage-ebook-download">
                            <a class="ebook-download-button" href="pdf_books/courses/intro-to-ada-ru.pdf">
                                PDF
                            </a>

                            <a class="ebook-download-button" href="epub_books/courses/intro-to-ada-ru.epub">
                                EPUB
                            </a>

                            <a class="ebook-download-button" href="mobi_books/courses/intro-to-ada-ru.mobi">
                                MOBI
                            </a>
                        </div>

    -------------

    Профессиональное обучение Аде
    ------------------------------

    **Пройдите профессиональное обучение по Аде** в
    `Adacore <https://www.adacore.com/training>`_.

    -------------

    Скачать инструментарий для Ады и SPARK
    --------------------------------------

    .. container:: download-button

        .. image:: images/GNAT-Community-download.png
            :target: https://www.adacore.com/download
            :alt: GNAT Community Download
            :width: 100pc

    **Попробуйте Аду и SPARK используя GNAT Community edition.**

    GNAT Community содержит компилятор языка Ада, инструменты SPARK и GNAT Studio IDE.

    --------------

    Академическая программа GNAT
    ----------------------------

    **Teachers and graduate students** who are interested in teaching or using Ada or SPARK can take
    advantage of AdaCore's `GNAT Academic Program (GAP) <http://www.adacore.com/academia>`_.

    .. container:: gap-logo

        .. image:: images/gap_logo.png
            :target: http://www.adacore.com/academia
            :alt: GNAT Academic Program
            :width: 100pc

    GAP's primary objective is to help put Ada and SPARK at the forefront of university study by
    building a community of academic professionals. GAP members receive a comprehensive
    toolset and professional support package specifically designed to provide the tools
    needed to teach and use Ada and SPARK in an academic setting. Best of all, AdaCore
    provides the GAP Package to eligible members at no cost.
    `Register <https://www.adacore.com/academia/gap-registration>`_ for membership
    today and join over 100 member universities in 35 countries currently teaching
    Ada and SPARK using GAP.
