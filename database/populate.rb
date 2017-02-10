require 'pg'
require 'faker'

min_interests = 0
max_interests = 6
user_count = 1_000_000

conn = PG.connect({user: 'webbench', dbname: 'webbench'})

conn.exec("TRUNCATE TABLE userinterests RESTART IDENTITY");
conn.exec("TRUNCATE TABLE interests RESTART IDENTITY");
conn.exec("TRUNCATE TABLE users RESTART IDENTITY");

conn.prepare("interests", "INSERT INTO interests (name) VALUES ($1)")

[
  'app.name',
  'beer.name',
  'book.title',
  'game_of_thrones.characters',
  'game_of_thrones.dragons',
  'harry_potter.characters',
  'harry_potter.books',
  'lord_of_the_rings.characters',
  'pokemon.names',
  'twin_peaks.characters',
  'rock_band.name',
  'rick_and_morty.characters'
].each do |key|
  Faker::Base.fetch_all(key).each do |interest|
    puts "Adding an interest of #{interest} from #{key}\n"
    conn.exec_prepared("interests", [interest])
  end
end

Faker::StarWars.characters.each do |character|
  puts "Adding an interest of #{character} from Star Wars characters\n"
  conn.exec_prepared("interests", [character])
end

conn.prepare "users", <<-HERE
  INSERT INTO users
  (firstname, middlename, lastname, title, streetaddress, city, state, zipcode, phonenumber, age)
  VALUES
  ($1,        $2,         $3,       $4,    $5,            $6,   $7,    $8,      $9,          $10)
HERE
user_count.times do |i|
  user = [
    Faker::Name.first_name,
    Faker::Name.first_name,
    Faker::Name.last_name,
    Faker::Name.title,
    Faker::Address.street_address,
    Faker::Address.city,
    Faker::Address.state_abbr,
    Faker::Address.zip[0,5],
    Faker::PhoneNumber.phone_number,
    Faker::Number.between(1, 100)
  ]
  puts "Inserting user ##{i+1}"
  conn.exec_prepared("users", user)
end

conn.prepare "get_interests", <<-HERE
  SELECT id
  FROM interests TABLESAMPLE SYSTEM (10)
  ORDER BY RANDOM()
  LIMIT $1
HERE

conn.prepare "add_interest", <<-HERE
  INSERT INTO userinterests (userid, interestid) VALUES ($1, $2)
HERE
conn.exec("SELECT id FROM users ORDER BY id").values.flatten.each do |user_id|
  puts "Adding interests to user id #{user_id}"
  interest_count = Faker::Number.between(min_interests, max_interests)
  conn.exec_prepared("get_interests", [interest_count]).values.flatten.each do |interest_id|
    conn.exec_prepared("add_interest", [user_id, interest_id])
  end
end

conn.exec("SELECT * FROM userview").values.each do |result|
  puts "#{result}\n"
end
