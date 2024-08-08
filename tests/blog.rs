
pub mod blog {
    use anyhow::Result;
    use std::iter::once as iter_once;
    use std::iter::Once as OnceIter;
    use std::ops::Not as _;
    use std::result::Result as StdResult;
    use std::slice::Iter as SliceIter;
    use string_interner::DefaultSymbol;
    use tagbuddy::generate_label;
    use tagbuddy::parse::*;
    use tagbuddy::storage::Storage;
    use tagbuddy::tag::KeyValueTag;
    use tagbuddy::tag::PlainTag;
    use tagbuddy::tag::Tagged;
    use tagbuddy::TagManager;

    generate_label! {
        pub Tags {}
        pub Ratings {}
    }

    type PostTagsManager = TagManager<Tags, DefaultSymbol, PlainTag<Tags>, Plain<Tags>>;
    type PostRatingsManager =
        TagManager<Ratings, DefaultSymbol, KeyValueTag<Ratings>, KeyValue<Ratings>>;

    pub struct Blog {
        posts: Vec<BlogPost>,
        tag_manager: PostTagsManager,
        rating_manager: PostRatingsManager,
    }

    impl Blog {
        /// Initialize a new blog.
        pub fn new() -> Self {
            let tag_manager = TagManager::builder()
                .parser(Plain::new())
                .storage(Storage::<Tags>::fresh())
                .build();

            let rating_manager = TagManager::builder()
                .parser(KeyValue::new(KvPolicy::NoAmbiguousSep))
                .storage(tag_manager.storage().shallow_clone::<Ratings>())
                .build();

            Self {
                posts: Vec::new(),
                tag_manager,
                rating_manager,
            }
        }

        /// Add a new post to the blog.
        pub fn add_post(
            &mut self,
            title: &str,
            content: &str,
            tags: &[&str],
            rating: &str,
        ) -> Result<&mut Self> {
            let title = title.to_owned();
            let content = content.to_owned();

            let tags = self
                .tag_manager
                .parse_tags_into::<StdResult<_, _>>(tags.into_iter().map(|t| *t))?;

            let rating = self.rating_manager.parse_tag(rating)?;

            self.posts.push(BlogPost {
                title,
                content,
                tags,
                rating,
            });

            Ok(self)
        }

        /// Get the posts in the blog.
        pub fn posts(&self) -> impl Iterator<Item = &BlogPost> {
            self.posts.iter()
        }
    }

    /// A single post on the blog.
    pub struct BlogPost {
        /// The title of the post.
        #[allow(unused)]
        title: String,

        /// The content of the post.
        #[allow(unused)]
        content: String,

        /// The tags associated with the post.
        tags: Vec<PlainTag<Tags>>,

        /// The rating assigned to the post.
        rating: KeyValueTag<Ratings>,
    }

    impl BlogPost {
        /// Get the tags applied to a blog post.
        pub fn tags(&self, blog: &Blog) -> Vec<String> {
            // SAFETY: We know we're using the correct storage, so the tag data should always be valid.
            blog.tag_manager
                .resolve_tags_into::<StdResult<_, _>>(Tagged::<PlainTag<Tags>>::get_tags(self))
                .expect("tags should always resolve successfully")
        }

        /// Get the rating of a blog post.
        pub fn rating(&self, blog: &Blog) -> String {
            // SAFETY: We know we're using the correct storage, so the rating data should always be valid.
            blog.rating_manager
                .resolve_tags_into::<StdResult<_, _>>(Tagged::<KeyValueTag<Ratings>>::get_tags(
                    self,
                ))
                .expect("ratings should always resolve successfully")
        }
    }

    // Mark a blog post as being tagged with tags.
    impl Tagged<PlainTag<Tags>> for BlogPost {
        type TagIter<'iter> = SliceIter<'iter, PlainTag<Tags>>;

        fn has_tags(&self) -> bool {
            self.tags.is_empty().not()
        }

        fn get_tags(&self) -> Self::TagIter<'_> {
            self.tags.iter()
        }
    }

    // Mark a blog post as being tagged with a rating.
    impl Tagged<KeyValueTag<Ratings>> for BlogPost {
        type TagIter<'iter> = OnceIter<&'iter KeyValueTag<Ratings>>;

        fn has_tags(&self) -> bool {
            true
        }

        fn get_tags(&self) -> Self::TagIter<'_> {
            iter_once(&self.rating)
        }
    }
}

use crate::blog::Blog;
use anyhow::Result;

#[test]
fn blog_can_handle_tags_and_rating() -> Result<()> {
    let mut blog = Blog::new();

    blog.add_post("one", "1", &["hello", "my", "friend"], "score:1")?
        .add_post("two", "2", &["goodbye", "your", "enemy"], "score:2")?
        .add_post(
            "three",
            "3",
            &["see you soon", "our", "acquaintance"],
            "score:3",
        )?;

    assert_eq!(
        blog.posts()
            .flat_map(|post| post.tags(&blog))
            .collect::<Vec<_>>(),
        vec![
            "hello",
            "my",
            "friend",
            "goodbye",
            "your",
            "enemy",
            "see you soon",
            "our",
            "acquaintance",
        ]
    );

    assert_eq!(
        blog.posts()
            .map(|post| post.rating(&blog))
            .collect::<Vec<_>>(),
        vec!["score:1", "score:2", "score:3"]
    );

    Ok(())
}
