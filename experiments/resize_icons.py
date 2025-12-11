#!/usr/bin/env python3
"""
Скрипт для изменения размера иконок для toolbar
Преобразует большие PNG изображения (256x256) в оптимальные размеры для toolbar
"""

from PIL import Image
import os
import sys

def resize_icon(input_path, output_path, size):
    """
    Изменяет размер изображения с сохранением прозрачности

    Args:
        input_path: путь к исходному изображению
        output_path: путь для сохранения результата
        size: кортеж (ширина, высота) нового размера
    """
    try:
        # Открываем изображение
        img = Image.open(input_path)

        # Проверяем, есть ли прозрачность
        if img.mode in ('RGBA', 'LA') or (img.mode == 'P' and 'transparency' in img.info):
            # Изменяем размер с сохранением качества
            # LANCZOS (ранее ANTIALIAS) - лучший алгоритм для уменьшения
            img_resized = img.resize(size, Image.Resampling.LANCZOS)
        else:
            # Если нет прозрачности, конвертируем в RGBA для совместимости
            img = img.convert('RGBA')
            img_resized = img.resize(size, Image.Resampling.LANCZOS)

        # Сохраняем с оптимизацией
        img_resized.save(output_path, 'PNG', optimize=True)
        print(f"✓ Создан: {output_path} ({size[0]}x{size[1]})")

        return True
    except Exception as e:
        print(f"✗ Ошибка при обработке {input_path}: {e}")
        return False

def main():
    """Основная функция для изменения размера иконок spreadsheet"""

    # Базовая директория с изображениями
    base_dir = "/tmp/gh-issue-solver-1765440168208"
    images_dir = os.path.join(
        base_dir,
        "environment/runtimefiles/AllCPU-AllOS/common/data/images/actions/velec"
    )

    # Список файлов для обработки
    files_to_resize = [
        "spreadsheet_calc.png",
        "spreadsheet_autocalc.png"
    ]

    # Размеры для создания (стандартные размеры для toolbar)
    sizes = [
        (16, 16),  # Основной размер для toolbar
        (24, 24),  # Для средних DPI
        (32, 32),  # Для высоких DPI
    ]

    print("=" * 70)
    print("Изменение размера иконок для toolbar электронных таблиц")
    print("=" * 70)

    success_count = 0
    total_count = 0

    for filename in files_to_resize:
        input_path = os.path.join(images_dir, filename)

        # Проверяем существование файла
        if not os.path.exists(input_path):
            print(f"⚠ Файл не найден: {input_path}")
            continue

        # Получаем информацию об исходном файле
        with Image.open(input_path) as img:
            original_size = img.size
            print(f"\n📁 Обработка: {filename}")
            print(f"   Исходный размер: {original_size[0]}x{original_size[1]}")

        # Создаём резервную копию оригинального файла
        backup_path = input_path.replace('.png', '_original_256.png')
        if not os.path.exists(backup_path):
            with Image.open(input_path) as img:
                img.save(backup_path, 'PNG')
            print(f"   💾 Создана резервная копия: {os.path.basename(backup_path)}")

        # Создаём версии разных размеров
        for size in sizes:
            total_count += 1

            # Для размера 16x16 заменяем оригинальный файл
            if size == (16, 16):
                output_path = input_path
            else:
                # Для других размеров создаём отдельные файлы
                output_path = input_path.replace('.png', f'_{size[0]}.png')

            if resize_icon(input_path if size == (16, 16) else backup_path,
                          output_path, size):
                success_count += 1

    print("\n" + "=" * 70)
    print(f"Готово! Успешно обработано: {success_count}/{total_count}")
    print("=" * 70)

    # Показываем результаты
    print("\n📊 Результаты:")
    for filename in files_to_resize:
        base_name = filename.replace('.png', '')
        print(f"\n{filename}:")
        for size in sizes:
            if size == (16, 16):
                file_path = os.path.join(images_dir, filename)
            else:
                file_path = os.path.join(images_dir, f"{base_name}_{size[0]}.png")

            if os.path.exists(file_path):
                file_size = os.path.getsize(file_path)
                print(f"   ✓ {size[0]}x{size[1]}: {file_path} ({file_size} bytes)")

if __name__ == "__main__":
    main()
