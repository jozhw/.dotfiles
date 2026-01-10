// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";

// https://astro.build/config
export default defineConfig({
    integrations: [
        starlight({
            title: "Dotfiles",
            social: [
                {
                    icon: "github",
                    label: "GitHub",
                    href: "https://github.com/jozhw/.dotfiles",
                },
            ],
            sidebar: [
                {
                    label: "Overview",
                    autogenerate: { directory: "overview" },
                },
                {
                    label: "Emacs",
                    items: [
                        {
                            label: "Inspirations",
                            slug: "emacs/inspirations-of-this-emacs-configuration",
                        },
                        {
                            label: "Getting Started",
                            slug: "emacs/getting-started",
                        },
                        {
                            label: "Early Initialization",
                            slug: "emacs/the-early-initialization-of-emacs-early-initel",
                        },
                        {
                            label: "Main Initialization",
                            slug: "emacs/the-main-initialization-of-emacs-initel",
                        },
                        {
                            label: "Modules",
                            slug: "emacs/the-modules-of-my-emacs-configuration-jw-emacs-modules",
                        },
                        {
                            label: "Custom Libraries",
                            slug: "emacs/the-custom-libraries-of-my-emacs-configuration-jw-lisp",
                        },
                    ],
                },
                {
                    label: "Development",
                    autogenerate: { directory: "development" },
                },
                {
                    label: "Configurations",
                    autogenerate: { directory: "configurations" },
                },
                {
                    label: "Atzlan",
                    autogenerate: { directory: "atzlan" },
                },

                {
                    label: "Garage",
                    autogenerate: { directory: "garage" },
                },
                {
                    label: "Reference",
                    autogenerate: { directory: "reference" },
                },
            ],
        }),
    ],
});
