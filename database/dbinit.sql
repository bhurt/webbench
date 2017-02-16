CREATE TABLE IF NOT EXISTS users (
    id BIGSERIAL PRIMARY KEY,
    firstName VARCHAR NOT NULL,
    middleName VARCHAR NOT NULL,
    lastName VARCHAR NOT NULL,
    title VARCHAR NOT NULL,
    streetAddress VARCHAR NOT NULL,
    city VARCHAR NOT NULL,
    state VARCHAR NOT NULL,
    zipcode VARCHAR(5) NOT NULL,
    phoneNumber VARCHAR NOT NULL,
    age INTEGER NOT NULL);

CREATE INDEX ON users (firstName);
CREATE INDEX ON users (lastName);
CREATE INDEX ON users (city);
CREATE INDEX ON users (state);
CREATE INDEX ON users (zipcode);
CREATE INDEX ON users (age);

CREATE TABLE IF NOT EXISTS interests (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR NOT NULL UNIQUE);

CREATE TABLE IF NOT EXISTS userInterests (
    userid BIGINT NOT NULL REFERENCES users(id) ON DELETE RESTRICT ON UPDATE CASCADE,
    interestid BIGINT NOT NULL REFERENCES interests(id) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE UNIQUE INDEX ON userInterests (userid, interestid);

CREATE VIEW userView AS
    SELECT
        u.id AS "id",
        u.firstName AS "firstName",
        u.middleName AS "middleName",
        u.lastName AS "lastName",
        u.title AS "title",
        streetAddress AS "streetAddress",
        u.city AS "city",
        u.state AS "state",
        u.zipcode AS "zipcode",
        u.phoneNumber AS "phoneNumber",
        u.age AS "age",
        array_agg(i.name) AS "interests"
    FROM
            users AS u
        LEFT JOIN
            userInterests AS ui
                ON u.id = ui.userid
        LEFT JOIN
            interests AS i
                ON ui.interestid = i.id
    GROUP BY
        u.id,
        u.firstName,
        u.middleName,
        u.lastName,
        u.title,
        u.streetAddress,
        u.city,
        u.state,
        u.zipcode,
        u.phoneNumber,
        u.age;
