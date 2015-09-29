CREATE TABLE books (
    id serial PRIMARY KEY,
    title text NOT NULL,
    short text NOT NULL,
    author text NOT NULL,
    year integer NOT NULL,
    created_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE recipes (
    id serial PRIMARY KEY,
    name text NOT NULL,
    book_id integer REFERENCES books(id),
    page_number integer,
    instructions text NOT NULL default '',
    total_time float NOT NULL,
    active_time float NOT NULL,
    number_servings integer NOT NULL,
    complexity integer NOT NULL,
    created_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE ingredients (
    id serial PRIMARY KEY,
    name text NOT NULL,
    grams_per_cc integer,
    created_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE recipe_ingredients (
    recipe_id integer NOT NULL REFERENCES recipes(id) ON DELETE CASCADE,
    ingredient_id integer NOT NULL REFERENCES ingredients(id) ON DELETE RESTRICT,
    units text NOT NULL,
    quantity float NOT NULL,
    original_text text NOT NULL,
    created_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE short_urls (
  short text PRIMARY KEY,
  url text NOT NULL
);
