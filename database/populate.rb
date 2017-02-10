require 'pg'
require 'faker'

conn = PG.connect({user: 'webbench', dbname: 'webbench'})

conn.exec("TRUNCATE TABLE interests RESTART IDENTITY");
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
  'twin_peaks.characters'
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
