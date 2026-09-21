pub mod blog {
    use anyhow::Result;
    use std::iter::once as iter_once;
    use std::iter::Once as OnceIter;
    use std::ops::Not as _;
    use std::result::Result as StdResult;
    use std::slice::Iter as SliceIter;
    use tagbuddy::brand::Guard;
    use tagbuddy::generate_label;
    use tagbuddy::parse::*;
    use tagbuddy::storage::Spur;
    use tagbuddy::storage::Storage;
    use tagbuddy::tag::KeyValueTag;
    use tagbuddy::tag::PlainTag;
    use tagbuddy::tag::Tagged;
    use tagbuddy::TagManager;

    generate_label! {
        pub Tags {}
        pub Ratings {}
    }

    type PostTagsManager<'brand> =
        TagManager<'brand, Tags, Spur, PlainTag<'brand, Tags>, Plain<Tags>>;
    type PostRatingsManager<'brand> =
        TagManager<'brand, Ratings, Spur, KeyValueTag<'brand, Ratings>, KeyValue<Ratings>>;

    pub struct Blog<'brand> {
        posts: Vec<BlogPost<'brand>>,
        tag_manager: PostTagsManager<'brand>,
        rating_manager: PostRatingsManager<'brand>,
    }

    impl<'brand> Blog<'brand> {
        /// Initialize a new blog.
        ///
        /// The `guard` brands the blog's storage, which is what ties every tag the blog
        /// produces to the interner that holds it.
        pub fn new(guard: Guard<'brand>) -> Self {
            let tag_manager = TagManager::builder()
                .parser(Plain::new())
                .storage(Storage::<Tags>::fresh(guard))
                .build();

            // The rating manager shares the tag manager's interner, so it shares its
            // brand too -- only the label differs, keeping the two vocabularies apart.
            let rating_manager = TagManager::builder()
                .parser(KeyValue::new(KvPolicy::NoAmbiguousSep))
                .storage(tag_manager.storage().share_as::<Ratings>())
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
                .parse_tags_into::<StdResult<_, _>>(tags.iter().copied())?;

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
        pub fn posts(&self) -> impl Iterator<Item = &BlogPost<'brand>> {
            self.posts.iter()
        }
    }

    /// A single post on the blog.
    pub struct BlogPost<'brand> {
        /// The title of the post.
        #[allow(unused)]
        title: String,

        /// The content of the post.
        #[allow(unused)]
        content: String,

        /// The tags associated with the post.
        tags: Vec<PlainTag<'brand, Tags>>,

        /// The rating assigned to the post.
        rating: KeyValueTag<'brand, Ratings>,
    }

    impl<'brand> BlogPost<'brand> {
        /// Get the tags applied to a blog post.
        pub fn tags(&self, blog: &Blog<'brand>) -> Vec<String> {
            // No error to handle: the brand says these tags came from this storage, and
            // the interner is append-only, so they're still there.
            blog.tag_manager
                .resolve_tags_into::<Vec<_>>(Tagged::<PlainTag<Tags>>::get_tags(self))
        }

        /// Get the rating of a blog post.
        pub fn rating(&self, blog: &Blog<'brand>) -> String {
            // Exactly one rating, so collecting the resolved tags into a `String`
            // yields it directly.
            Tagged::<KeyValueTag<Ratings>>::get_tags(self)
                .map(|tag| blog.rating_manager.resolve_tag(tag))
                .collect()
        }
    }

    // Mark a blog post as being tagged with tags.
    impl<'brand> Tagged<'brand, PlainTag<'brand, Tags>> for BlogPost<'brand> {
        type TagIter<'iter>
            = SliceIter<'iter, PlainTag<'brand, Tags>>
        where
            Self: 'iter;

        fn has_tags(&self) -> bool {
            self.tags.is_empty().not()
        }

        fn get_tags(&self) -> Self::TagIter<'_> {
            self.tags.iter()
        }
    }

    // Mark a blog post as being tagged with a rating.
    impl<'brand> Tagged<'brand, KeyValueTag<'brand, Ratings>> for BlogPost<'brand> {
        type TagIter<'iter>
            = OnceIter<&'iter KeyValueTag<'brand, Ratings>>
        where
            Self: 'iter;

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
use tagbuddy::brand::make_guard;

#[test]
fn blog_can_handle_tags_and_rating() -> Result<()> {
    make_guard!(guard);
    let mut blog = Blog::new(guard);

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
