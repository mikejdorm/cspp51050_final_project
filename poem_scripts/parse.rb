require 'rubygems'
require 'nokogiri'   
require 'open-uri'
require 'mysql'

begin
  	db = Mysql.new('localhost', 'username', 'password', 'poet_tree')
rescue Mysql::Error
  puts "Unable to connect to the database"
  exit 1
end

	Dir["../poems_archive/*"].each{ |file|
		page = Nokogiri::HTML(open(file))
		poem =  page.css('div.poem').text
		author =  page.css('span.author a').text
		title = page.css('div#poem-top h1').text
		
		    st = db.prepare("SELECT id FROM authors WHERE name = ?")
 			st.execute author
  			r = st.fetch
				if r == nil
					begin
						  insert_new_user = db.prepare "INSERT INTO authors (name) VALUES (?)"
						  insert_new_user.execute author
						  insert_new_user.close
					rescue Mysql::Error
							puts 'error when adding author'
					end
				end
  			st.close
  		
		begin
  			st1 = db.prepare("SELECT id FROM authors WHERE name = ?")
 			st1.execute author
  			r1 = st1.fetch
  			puts "The author id is #{r1[0]} for author: #{author}"
  			puts "Adding poem #{author} : #{title}"
  			puts "HEre's the poem: #{poem.to_s}"
		  insert_new_poem = db.prepare "INSERT INTO poetry (author_id, poem_text, title) VALUES (?, ? ,?)"
		  insert_new_poem.execute r1[0], poem.to_s.strip!, title 
		  insert_new_poem.close
		 	st1.close
		rescue Mysql::Error
			puts 'error when adding poem ' + $!.message
		end
		
	}

