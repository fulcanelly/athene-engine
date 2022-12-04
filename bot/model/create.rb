
class CreateAll < ActiveRecord::Migration[7.0]
    def change 

        #telegram user 
        create_table :users, if_not_exists: true do |t|
                    
            t.string :name
            t.integer :user_id
            
            t.timestamps 
        end
        
        #state of bot
        create_table :states, if_not_exists: true do |t|
            t.binary :state_dump 

            t.references :user, null: true, foreign_key: { to_table: :users }

            t.timestamps
        end
    
        # recorded action of state 
        create_table :actions, if_not_exists: true do |t|
            t.references :user, foreign_key: { to_table: :users }
            t.string :name
            t.binary :data
            t.timestamps
        end

        # where player currently lives 
        create_table :locations, if_not_exists: true do |t|
            t.string :name
            t.string :type 
            t.timestamps 
        end

        #what player do for living
        create_table :occupations, if_not_exists: true do |t|
            t.string :name 
            t.string :type 
            t.timestamps 
        end

        #character is all about player 

        create_table :characters, if_not_exists: true do |t|
            t.string :name
            t.integer :age
            t.integer :karma 
            t.integer :deaths
            t.boolean :is_man?

            t.references :user, null: true, foreign_key: { to_table: :users }
            t.references :occupation, null: true, foreign_key: { to_table: :occupations }
            t.references :location, null: true, foreign_key: { to_table: :locations }

            t.timestamps 


        end 

        
    end
end


CreateAll.new.change