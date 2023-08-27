class FindPartnersState < BaseState

    #TODO: RateGroups(group users by thier tastes)

    def get_non_rated_post 

        #TODO Channels.where.not(liked),(not my).posts.most_liked
        Faker::Lorem.paragraph
    end

    def run 
       # say "searching partner"
        suggest_it(get_non_rated_post())
            .option("Like") do 
            end
            .option("Dislike") do 
                
            end
            .option("Back") do 
                switch_state MainMenuState.new
            end
            .exec
        switch_state FindPartnersState.new
    end
end
