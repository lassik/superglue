/*! Generator: superglue */

struct ff_sqlite3_db;
struct ff_sqlite3_stmt;
struct ff_error *ff_sqlite3_open(const char *filename, struct ff_sqlite3_db **out_db);
struct ff_error *ff_sqlite3_prepare(struct ff_sqlite3_db *db, const char *sql);
struct ff_error *ff_sqlite3_errstr(uint64_t number, const char **out_value);
struct ff_error *ff_sqlite3_errmsg(struct ff_sqlite3_db *db, const char **out_value);
struct ff_error *ff_sqlite3_set_last_insert_rowid(struct ff_sqlite3_db *db, int64_t rowid);
struct ff_error *ff_sqlite3_last_insert_rowid(struct ff_sqlite3_db *db, int64_t *out_rowid);
struct ff_error *ff_sqlite3_bind_parameter_count(struct ff_sqlite3_stmt *stmt, uint64_t *out_count);
struct ff_error *ff_sqlite3_bind_parameter_index(struct ff_sqlite3_stmt *stmt, const char *param, uint64_t *out_index);
struct ff_error *ff_sqlite3_bind_null(struct ff_sqlite3_stmt *stmt, uint64_t param);
struct ff_error *ff_sqlite3_bind_text(struct ff_sqlite3_stmt *stmt, uint64_t param, const char *value);
struct ff_error *ff_sqlite3_bind_int64(struct ff_sqlite3_stmt *stmt, uint64_t param, int64_t value);
struct ff_error *ff_sqlite3_bind_double(struct ff_sqlite3_stmt *stmt, uint64_t param, double value);
