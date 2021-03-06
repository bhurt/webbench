require 'pg'
require 'faker'
require 'csv'

min_interests = 0
max_interests = 10
user_count = 10_000_000

conn = PG.connect()

conn.exec("TRUNCATE userinterests, interests, users RESTART IDENTITY");

conn.copy_data "COPY interests (name) FROM STDIN CSV" do
  interests = [
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
  ].map do |key|
    Faker::Base.fetch_all(key).map do |interest|
      puts "Adding an interest of #{interest} from #{key}\n"
      interest
    end
  end

  Faker::StarWars.characters.each do |character|
    puts "Adding an interest of #{character} from Star Wars characters\n"
    interests.push(character)
  end

  interests.flatten!
  interests.uniq!
  interests.each do |interest|
    conn.put_copy_data [interest].to_csv
  end

end

conn.copy_data(
  <<-HERE
    COPY users
    (firstname, middlename, lastname, title, streetaddress, city, state, zipcode, phonenumber, age)
    FROM STDIN CSV
  HERE
) do
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
    conn.put_copy_data user.to_csv
  end
end

interest_ids = conn.exec("SELECT id FROM interests").values.flatten
interest_ids.shuffle!
user_ids = conn.exec("SELECT id FROM users ORDER BY id ASC").values.flatten
user_ids.shuffle!

conn.copy_data "COPY userinterests (userid, interestid) FROM STDIN CSV" do
  until user_ids.empty?
    user_id = user_ids.pop
    puts "Adding interests to user id #{user_id}"
    interest_count = Faker::Number.between(min_interests, max_interests)
    interest_count.times do
      interest_id = interest_ids.sample
      conn.put_copy_data [user_id, interest_id].to_csv
    end
  end
end

conn.exec("SELECT COUNT(*) FROM userview").each_row do |result|
  puts "Rows in the user view: #{result}\n"
end
