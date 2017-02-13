
CREATE TABLE IF NOT EXISTS users (
    id BIGSERIAL NOT NULL,
    firstName VARCHAR NOT NULL,
    middleName VARCHAR,
    lastName VARCHAR NOT NULL,
    title VARCHAR,
    streetAddress VARCHAR NOT NULL,
    city VARCHAR NOT NULL,
    state VARCHAR NOT NULL,
    zipcode VARCHAR(5) NOT NULL,
    phoneNumber VARCHAR,
    age INTEGER);

CREATE TABLE IF NOT EXISTS interests (
    id BIGSERIAL NOT NULL,
    name VARCHAR);

CREATE TABLE IF NOT EXISTS userInterests (
    userid BIGINT NOT NULL,
    interestid BIGINT NOT NULL
);

CREATE VIEW userView AS
    SELECT
        u.id,
        u.firstName,
        u.middleName,
        u.lastName,
        u.title,
        streetAddress,
        u.city,
        u.state,
        u.zipcode,
        u.phoneNumber,
        u.age,
        array_agg(i.name)
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
        streetAddress,
        u.city,
        u.state,
        u.zipcode,
        u.phoneNumber,
        u.age;