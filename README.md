# Система рекомендации фильмов

## Стек
- Язык программирования: R (tidyverse, caret, readr)
- Датасет: movielens (2019)

## Описание файлов
1. **data.R** : Импорт movielens датасета, отрезание хвостов, разбиение на тренировочную, валидационную и тестовую выборки.
2. **training.R** : Создание начальных таблиц (таблица средних рейтингов фильмов, таблица среднего отклонения от среднего пользователей, таблица отклонения от среднего по каждому пользователю и фильму).
3. **testing1.R** : Тестирование примитивной модели предсказания. *Ошибка: 0.860*
4. **movie_correlation.R** : Создание таблицы корреляций рейтингов фильмов.
5. **movie_corr_validation.R** : Валидация коэффициента для слагаемого в предсказательной модели, полученного с помощью таблицы корреляций рейтингов фильмов.
6. **testing2.R** : Тестирование модели со слагаемым, полученным с помощью таблицы корреляций рейтингов фильмов. *Ошибка: 0.791*
7. **user_corr_validation.R** : Построение модели со слагаемым, полученного с помощью рассчёта корреляций пользователей. Валидация коэффициента для слагаемого в модели.
8. **double_coef_val.R** : Нахождение оптимальной пары коэффициентов для полной модели предсказания.
9. **testing_final.R** : Тестирование финальной модели. *Ошибка: 0.786*.
10. **predict.R** : Рекомендация фильмов по введённой пользователем таблице рейтингов.