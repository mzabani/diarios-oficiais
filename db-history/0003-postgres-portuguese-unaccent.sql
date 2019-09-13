CREATE EXTENSION IF NOT EXISTS unaccent;
alter text search configuration portuguese alter mapping for word, hword, hword_part with portuguese_stem, unaccent;