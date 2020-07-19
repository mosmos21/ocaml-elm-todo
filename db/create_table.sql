CREATE TABLE todo_app.todo (
    id         int          not null AUTO_INCREMENT,
    title      varchar(255) not null,
    content    varchar(255) not null,
    status     int          not null default 0,
    created_at datetime     not null default CURRENT_TIMESTAMP,
    updated_at datetime     not null default CURRENT_TIMESTAMP,
    PRIMARY KEY (id)
);